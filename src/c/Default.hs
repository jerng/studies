{-# LANGUAGE OverloadedStrings #-} 

module Controllers.Default where

import qualified Data.Text as T 
import Data.Dynamic
import Hell.Types
import Network.Wai
import Network.HTTP.Types

index :: Request -> AppControllerVars -> Reaction
index request appControllerVariables = 
  Reaction status200 ("default","index")
    [ ("someVar", T.pack $ show 999) ]
