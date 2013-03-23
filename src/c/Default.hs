index :: Request -> AppControllerVars -> Reaction
index request appControllerVariables = 
  Reaction ok200 ("default","index")
    [ ("someVar", T.pack $ show 999) ]
