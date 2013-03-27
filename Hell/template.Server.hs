{-# LANGUAGE OverloadedStrings #-} 

import Hell.Lib
import qualified Data.Text as T

import qualified AppController

{-makeHell:ImportControllers-}

{-makeHell:ImportViews-}

main :: IO ()
main = run hellServerPort app

app :: Request -> ResourceT IO Response 
app = \request-> render $ report request 

report ::  Request -> Report
report request' = 
  let route = router request'
      (routeA', action) = confirmAction route 
      initialReport = defaultReport 
        { request = Just request'
        , actionDictionary = appControllerVariables
        , routeA = routeA'
        } 
  in  applyActionToReport action initialReport 

confirmAction :: Route -> (Route, Action)
confirmAction route = case lookup route actionList of
  Just action -> (route, action)
  Nothing -> 
    ( Hell.Lib.noSuchActionRoute
    , fromJust $ lookup (Hell.Lib.noSuchActionRoute) actionList
    ) 
    -- Perhaps unnecessarily wordy?
    -- Does GHC optimise-away messes like this?
    -- ANYWAY: do what CakePHP calls a "setFlash" here.


applyActionToReport :: Action -> Report -> Report
applyActionToReport action report = AppController.main report action

applyActionToSubReport :: Action -> Report -> Report
applyActionToSubReport action report = AppController.subMain report action

router :: Request -> Route 
router request = 
  case pathInfo request of
    []    -> Hell.Lib.defaultRoute
    c:[]  -> ( T.toLower c, Hell.Lib.indexAction )
    c:a:_ -> ( T.toLower c, T.toLower a )

{- for reference:

data Response = 
ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)	 
ResponseBuilder Status ResponseHeaders Builder	 
ResponseSource Status ResponseHeaders (Source (ResourceT IO) (Flush Builder))	

Hell.Server.(render) should have branches to deal with all the above, eventually.
FilePath, FilePart, Source, etc. should be passed in the ViewDictionary.
(render) should be able to determine, from the Report, which data constructor
of Response is required by the Controller.

ResponseHeaders are currently hardcoded here as [].
Later, there should be a mechanism for updating the
ResponseHeaders based on the Report from the Action.

-}

-- | Takes Report from a Controller, returns a variety of ResponseBuilder.
render :: Report -> ResourceT IO Response
render report = 
  let getTexts (key, subReport) = 
        let ( route, action ) = confirmAction $ routeA subReport 
        in  ( key
            , toDyn 
              ( ( reportToText $ applyActionToSubReport action subReport ) :: Text )
            )
      report' = case subReports report of
        []          -> report
        subReports  -> report 
          { subReports = []
          , viewDictionary = 
              (viewDictionary report) ++ (map getTexts subReports)
          }
  in  return $ 
      ResponseBuilder (status report') [] $ fromText $ 
        
      -- SOFTEN CODE HERE: there are other types of ResponseBuilders
      case viewTemplate report of
        Nothing -> reportToText report'
        Just route -> reportToText report' -- rendered outer view
          { viewTemplate = Nothing
          , routeV = route
          , viewDictionary = 
            ( "viewContent"
            , toDyn (reportToText report') -- rendered inner view
            ):viewDictionary report' 
          }

-- | Takes Report from a Controller, returns a Text.
reportToText :: Report -> Text
reportToText report = fromMaybe
  (fromJust $ lookup Hell.Lib.noSuchViewRoute viewList)
  (lookup (routeV report ) viewList)
  report

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

