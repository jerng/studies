index :: Request -> AppControllerVars -> Reaction
index request appControllerVariables = 
  Reaction ok200 ("default","index")
    [ ("someInt", toDyn (999 :: Int)) 
    , ("someFloat", toDyn (3.142 :: Float)) 
    , ("someText", toDyn ("I_AM_TEXT" :: Text)) 
    , ("someIntList", toDyn ([1,2,3]::[Int]))
    ]

    --  TODO :Try to make a helper function for sending variables to the
    --  ViewableList.
    --
    --  TODO :Test if it is performant to pass variables using
    --  (show) and (read) instead of Dynamic. Doing so might reduce
    --  boilerplate and the need for utility functions. But I am
    --  concerned that it will run slowly.
