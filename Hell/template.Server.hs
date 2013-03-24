{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TemplateHaskell #-}

import Hell.Lib
import qualified Data.Text as T

{-assemble:ImportControllers-}

{-assemble:ImportViews-}

main :: IO ()
main = run hellServerPort app

app :: Request -> ResourceT IO Response 
app = \request-> render $ getActor request 

getActor ::  Request -> Action
getActor request = actor request appControllerVariables
  where actor = 
          case lookup (router request) actorList of
            Just a -> a
            Nothing -> fromJust $ lookup (Hell.Lib.defaultRoute) actorList
                        -- Perhaps unnecessarily wordy...?
                        -- Does GHC optimise out things like this?
                        --
                        -- ANYWAY: do what CakePHP calls a "setFlash" here.

-- | Convert from pattern guard, to let {} in { case of }
router :: Request -> Route 
router request 
  | [] <- pathInfo request = Hell.Lib.defaultRoute
  -- (Hell.Conf.customPaths) may be defined, then called here.
  | c:a:_ <- pathInfo request = ( T.toLower c, T.toLower a )

render :: Action -> ResourceT IO Response
render (Action status route textMap) = return $
  ResponseBuilder status [] $ -- customisation of ResponseHeaders should occur here
  fromText $
  maybe 
  "We lack a point of View." 
  ( \view->view (Action status route textMap) )
  ( lookup route viewList ) 

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

actorList :: [(Route, Request -> AppControllerVars -> Action)]
actorList = [
        (("default", "index"), Controllers.Default.index)
    ]

viewList :: [(Route, Action -> Text)]
viewList = [
        (("default", "index"), Views.Default.Index.main)
    ]

