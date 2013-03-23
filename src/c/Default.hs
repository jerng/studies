index :: Request -> AppControllerVars -> Action
index request appControllerVariables = 
  Action ok200 ("default","index")
    [ ("someInt", toDyn (999 :: Int)) 
    , ("someFloat", toDyn (3.142 :: Float)) 
    , ("someText", toDyn ("I_AM_TEXT" :: Text)) 
    , ("someIntList", toDyn ([1,2,3]::[Int]))
    ]

    --  TODO :Try to make a helper function for sending variables to the
    --  ViewableList.
