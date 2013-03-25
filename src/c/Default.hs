index :: Request -> AppControllerVars -> Report
index request appControllerVariables = 
  Report ok200 ("default","index")
    [ ("someInt", toDyn (999 :: Int)) 
    , ("someFloat", toDyn (3.142 :: Float)) 
    , ("someText", toDyn ("I_AM_TEXT" :: Text)) 
    , ("someIntList", toDyn ([1,2,3]::[Int]))
    ]

    --  TODO :Try to make a helper function for sending variables to the
    --  ViewableList.

index2 :: Request -> AppControllerVars -> Report
index2 request appControllerVariables = 
  Report ok200 ("default","index2") []


nosuchaction :: Request -> AppControllerVars -> Report
nosuchaction request appControllerVariables =
  Report ok200 ("default","nosuchaction") []
