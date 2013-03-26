index :: Report -> Report
index report = 
  report
  { status = ok200
  , routeV = ("default","index")
  , viewDictionary = 
    [ ("someInt", toDyn (999 :: Int)) 
    , ("someFloat", toDyn (3.142 :: Float)) 
    , ("someText", toDyn ("I_AM_TEXT" :: Text)) 
    , ("someIntList", toDyn ([1,2,3]::[Int]))
    ]
  }
    --  TODO :Try to make a helper function for sending variables to the
    --  ViewableList.

index2 :: Report -> Report
index2 report = 
  report
  { status = ok200
  , routeV = ("default","index2")
  }


nosuchaction :: Report -> Report
nosuchaction report =
  report
  { status = ok200
  , routeV = ("default","nosuchaction")
  }
