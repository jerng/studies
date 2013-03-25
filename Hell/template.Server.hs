{-# LANGUAGE OverloadedStrings #-} 

import Hell.Lib
import qualified Data.Text as T

{-assemble:ImportControllers-}

{-assemble:ImportViews-}

main :: IO ()
main = run hellServerPort app

app :: Request -> ResourceT IO Response 
app = \request-> render $ getReport request 

-- | "AppController" would have to intercept, here.
getReport ::  Request -> Report
getReport request = action request appControllerVariables
  where 
    action = 
      case lookup (router request) actionList of
        Just a -> a
        Nothing -> fromJust $ 
          lookup (Hell.Lib.noSuchActionRoute) actionList
            -- Perhaps unnecessarily wordy...?
            -- Does GHC optimise out things like this?
            --
            -- ANYWAY: do what CakePHP calls a "setFlash" here.

router :: Request -> Route 
router request = 
  case pathInfo request of
    []    -> Hell.Lib.defaultRoute
    c:a:_ -> ( T.toLower c, T.toLower a )

render :: Report -> ResourceT IO Response
render (Report status (c,a) textMap) = return $
  ResponseBuilder status [] $ 
    -- customisation of ResponseHeaders should occur here
  fromText $
  maybe 
  "We lack a point of View." 
  ( \view->view (Report status (c,a) textMap) )
  ( lookup (c,a) viewList ) 

-- | SHOULD THIS GO INTO (Hell.Conf) ?
-- TO CONSULT PROFESSIONALS:
-- Getting some strange compiler behaviour here. The code:
--    do return $ head appControllerVariables
-- returns [appControllerVariables] instead of appControllerVariables.
--
-- Using Text data as keys. Using ADTs revealed complications. That may be 
-- reattempted in the future.
appControllerVariables :: AppControllerVars 
appControllerVariables = [
        (   "someVar1",
            toDyn ("\"Hello, I'm defined in AppController\"" :: Text))
        ,(  "someVar2",
            toDyn ("\"Hello, I too am defined in AppController\"" :: Text ))
    ]

actionList :: [(Route, Request -> AppControllerVars -> Report)]
actionList = 
  [ {-assemble:ListActions-}
  ]

viewList :: [(Route, Report -> Text)]
viewList = 
  [ {-assemble:ListViews-}
  ]

