{-# LANGUAGE OverloadedStrings #-} 

module Controllers.Default where

import qualified Data.Text as T 
import Hell.Lib

index :: Request -> AppControllerVars -> Reaction
index request appControllerVariables = 
  Reaction ok200 ("default","index")
    [ ("someVar", T.pack $ show 999) ]
