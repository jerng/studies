{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TemplateHaskell #-}

import Hell.Lib
import qualified Data.Text as T

{-assemble:ImportControllers-}

{-assemble:ImportViews-}

main :: IO ()
main = run 3000 app

app :: Request -> ResourceT IO Response 
app request =  render $ act request 

act ::  Request -> Reaction
act request 
  = ( maybe 
      ( fromJust $ lookup (Hell.Lib.defaultRoute) actionList ) 
      id
      ( lookup (router request) actionList ) 
    )
    request 
    appControllerVariables
    
-- | Convert from pattern guard, to let {} in { case of }
router :: Request -> Route 
router request 
  | [] <- pathInfo request = Hell.Lib.defaultRoute
  -- (Hell.Conf.customPaths) may be defined, then called here.
  | c:a:_ <- pathInfo request = ( c, a )

render :: Reaction -> ResourceT IO Response
render (Reaction status route textMap) = return $
   ResponseBuilder status [] $ -- customisation of ResponseHeaders occurs here
   fromText $

      -- OPTIMISATION: look into... working backwards, 
      -- and replacing as much (Text) as possible, with (Builder).
      -- Try to figure out how to start using Builder instead of Text in Views.
   
    

    maybe 
    "Missing View:" 
    ( \view->view (Reaction status route textMap) )
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



-- | CONTINUE HERE: Generate these from src0
--
-- Possible alternatives to using [((Text,Text),a)] :
--
-- (a) Data.Map 
-- (b) [(Text,a)]] 

actionList :: [(Route, Request -> AppControllerVars -> Reaction)]
actionList = [
        (("default", "index"), Controllers.Default.index)
    ]

viewList :: [(Route, Reaction -> Text)]
viewList = [
        (("default", "index"), Views.Default.Index.main)
    ]

