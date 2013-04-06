index :: Report -> Report
index report = 
  report
  { status = ok200
  , routeV = ("default","index")
  , viewBson = 
    
    [ "someInt" :=Int32 999
    , "someFloat" :=Float 3.132
    , "someText" :=String "I_AM_TEXT"
    , "someIntList" := Array [Int32 1,Int32 2, Int32 3]
    ]

--    [ ("someInt", toDyn (999 :: Int)) 
--    , ("someFloat", toDyn (3.142 :: Float)) 
--    , ("someText", toDyn ("I_AM_TEXT" :: Text)) 
--    , ("someIntList", toDyn ([1,2,3]::[Int]))
--    ]

  , subReports = [("innerkey", report {routeA = ("default", "inner")}) ] 
  , viewTemplate = Just ("default","template")
  }
    --  TODO :Try to make a helper function for sending variables to the
    --  ViewableList.

    -- TODO: have the rendered create a default routeV if it's not specified

index2 :: Report -> Report
index2 report = 
  report
  { status = ok200
  , routeV = ("default","index2")
  , viewTemplate = Nothing
  }


nosuchaction :: Report -> Report
nosuchaction report =
  report
  { status = ok200
  , routeV = ("default","nosuchaction")
  }


inner :: Report -> Report 
inner report =
  report
  { status = ok200
  , routeV = ("default","inner")
  }


