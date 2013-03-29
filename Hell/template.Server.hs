{-# LANGUAGE OverloadedStrings #-} 

import Hell.Lib
import qualified Data.Text as T

import qualified AppController

{-makeHell:ImportControllers-}

{-makeHell:ImportViews-}

main :: IO ()
main = run hellServerPort app

app :: Request -> ResourceT IO Response 
app = \request-> renderR $ getR request 

getR :: Request -> Report
getR r = 
  let initialReport = defaultReport { request = Just r } 
      (r',a') = confirmA $ router initialReport 
  in  aToR a' r'

confirmA :: Report -> (Report,Action)
confirmA r = 
  case lookup (routeA r) aList of
    Just a  -> (r, a)
    Nothing -> (r { routeA = Hell.Lib.noSuchActionRoute
                  , meta =  if    Hell.Lib.appMode == Development 
                            then  Hell.Lib.metaNoSuchAction 
                            else  meta r
                  }
               , fromJust $ lookup (Hell.Lib.noSuchActionRoute) aList
               ) 
                  -- Perhaps unnecessarily wordy?
                  -- Does GHC optimise-away messes like this?

aToR :: Action -> Report -> Report
aToR a r = AppController.main r a

aToSubR :: Action -> Report -> Report
aToSubR a r = AppController.subMain r a

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

Hell.Server.(renderR) should have branches to deal with all the above, eventually.
FilePath, FilePart, Source, etc. should be passed in the ViewDictionary.
(renderR) should be able to determine, from the Report, which data constructor
of Response is required by the Controller.

ResponseHeaders are currently hardcoded here as [].
Later, there should be a mechanism for updating the
ResponseHeaders based on the Report from the Action.

-}

-- | Takes Report from a Controller, returns a variety of ResponseBuilder.
renderR :: Report -> ResourceT IO Response
renderR r = 
  let subRtoT (key,subReport) = (key, toDyn $ rToT $ aToSubR a subR)
        where (subR,a) = confirmA subReport 

      r' = case subReports r of 
        [] -> r
        subRs -> r  { subReports = []
                    , viewDictionary = (viewDictionary r) ++ (map subRtoT subRs)
                    }

  in  return $ ResponseBuilder (status r') [] $ fromText $ 
        
      -- SOFTEN CODE HERE: there are other types of ResponseBuilders
      case viewTemplate r of
        Nothing -> rToT r'
        Just route -> rToT r' -- rendered outer view
          { viewTemplate = Nothing
          , routeV = route
          , meta = ""
          , viewDictionary =  
            (Hell.Lib.keyOfTemplatedView, toDyn $ rToT r') 
                    -- rendered inner view
            :(Hell.Lib.keyOfMetaView, toDyn $ meta r')
            :viewDictionary r' 
              -- TODO: soften these arguments.
          }

-- | Takes Report from a Controller, returns a Text.
rToT :: Report -> Text
rToT r = fromMaybe
  (fromJust $ lookup Hell.Lib.noSuchViewRoute vList)
  (lookup (routeV r) vList)
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

aList :: [(Route, Action)]
aList = 
  [ {-makeHell:ListActions-}
  ]

vList :: [(Route, Report -> Text)]
vList = 
  [ {-makeHell:ListViews-}
  ]

