{-# LANGUAGE OverloadedStrings #-} 

import Hell.Lib
import qualified Data.Text as T

import qualified AppController

{-makeHell:ImportControllers-}

{-makeHell:ImportViews-}

main :: IO ()
main = run hellServerPort app

app :: Request -> ResourceT IO Response 
app = \request-> renderReport $ getReport request 

getReport :: Request -> Report
getReport r = 
  let initialReport = defaultReport { request = Just r } 
      (r',a') = confirmAction $ router initialReport 
  in  applyActionToReport a' r'

confirmAction :: Report -> (Report,Action)
confirmAction r = 
  case lookup (routeA r) actionList of
    Just a  -> (r, a)
    Nothing -> (r { routeA = Hell.Lib.noSuchActionRoute
                  , meta =  if    Hell.Lib.appMode == Development 
                            then  Hell.Lib.metaNoSuchAction 
                            else  meta r
                      -- TODO: soften this argument 
                  }
               , fromJust $ lookup (Hell.Lib.noSuchActionRoute) actionList
               ) 
                  -- Perhaps unnecessarily wordy?
                  -- Does GHC optimise-away messes like this?

applyActionToReport :: Action -> Report -> Report
applyActionToReport a r = AppController.main r a

applyActionToSubReport :: Action -> Report -> Report
applyActionToSubReport a r = AppController.subMain r a

-- | Maybe add a hook from here, to Hell.Conf.
router :: Report -> Report
router r = 
  let r' = case pathInfo $ fromJust $ request r of
        []    -> Hell.Lib.defaultRoute
        c:[]  -> ( T.toLower c, Hell.Lib.indexAction )
        c:a:_ -> ( T.toLower c, T.toLower a )
  in  r { routeA = r' }
    
{- for reference:

data Response = 
ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)	 
ResponseBuilder Status ResponseHeaders Builder	 
ResponseSource Status ResponseHeaders (Source (ResourceT IO) (Flush Builder))	

Hell.Server.(renderReport) should have branches to deal with all the above, eventually.
FilePath, FilePart, Source, etc. should be passed in the ViewDictionary.
(renderReport) should be able to determine, from the Report, which data constructor
of Response is required by the Controller.

ResponseHeaders are currently hardcoded here as [].
Later, there should be a mechanism for updating the
ResponseHeaders based on the Report from the Action.

-}

-- | Takes Report from a Controller, returns a variety of ResponseBuilder.
renderReport :: Report -> ResourceT IO Response
renderReport r = 
  let sRtoT (key,subReport) = 
        let (sR,a) = confirmAction subReport 
        in  (key,toDyn $ reportToText $ applyActionToSubReport a sR)

      r' = case subReports r of
        []  -> r
        sRs -> r 
          { subReports = []
          , viewDictionary = 
            (viewDictionary r) ++ (map sRtoT sRs)
          }

  in  return $ 
      ResponseBuilder (status r') [] $ fromText $ 
        
      -- SOFTEN CODE HERE: there are other types of ResponseBuilders
      case viewTemplate r of
        Nothing -> reportToText r'
        Just route -> reportToText r' -- rendered outer view
          { viewTemplate = Nothing
          , routeV = route
          , meta = ""
          , viewDictionary =  (Hell.Lib.keyOfTemplatedView, toDyn $ reportToText r') 
                                      -- rendered inner view
                              :(Hell.Lib.keyOfMetaView, toDyn $ meta r')
                              :viewDictionary r' 
                                -- TODO: soften these arguments.
          }

-- | Takes Report from a Controller, returns a Text.
reportToText :: Report -> Text
reportToText r = fromMaybe
  (fromJust $ lookup Hell.Lib.noSuchViewRoute viewList)
  (lookup (routeV r) viewList)
  r

-- | SHOULD THIS GO INTO (Hell.Conf) ?

{- TO CONSULT PROFESSIONALS:
 Getting some strange compiler behaviour here. The code:
    do return $ head appControllerVariables
 returns [appControllerVariables] instead of appControllerVariables.

 Using Text data as keys. Using ADTs revealed complications. That may be 
 reattempted in the future.
-}

appControllerVariables :: ActionDictionary 
appControllerVariables = 
  [ ("someVar1", toDyn ("\"Hello, I'm defined in AppController\"" :: Text))
  , ("someVar2", toDyn ("\"Hello, I too am defined in AppController\"" :: Text))
  ]

actionList :: [(Route, Action)]
actionList = 
  [ {-makeHell:ListActions-}
  ]

viewList :: [(Route, Report -> Text)]
viewList = 
  [ {-makeHell:ListViews-}
  ]

