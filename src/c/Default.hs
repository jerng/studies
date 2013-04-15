index :: Report -> Report
index report = 
  "more debuggable stuff (because if you do, you have to type from right to left)" ?>> 
  "debuggable stuff (I don't recommend you use the (?>>) operator)" ?>> 

  report
  { status = ok200
  , viewRoute = ("default","index")
  , viewBson = 
    [ "someInt" :=Int32 999
    , "someFloat" :=Float 3.132
    , "someText" :=String "I_AM_TEXT"
    , "someIntList" := Array [Int32 1,Int32 2, Int32 3]
    ]
  , subReports = [("innerkey", report { actRoute = ("default", "inner") })] 
  , viewTemplate = Just ("default","template")
  , resCookies = defaultCookie 
    { cookieName = "newCookie"
    , cookieValue = "newValue"
    } : resCookies report
  }

      <<? "even more debuggable stuff"
      <<? "lastly, for good measure" 
      <<? "default.hs line 14" 
      <<? tPack.show...666 
      <<? "just trying to debug an int"
    --  TODO :Try to make a helper function for sending variables to the
    --  ViewableList.

    -- TODO: have the rendered create a default viewRoute if it's not specified

index2 :: Report -> Report
index2 report = 
  report
  { status = ok200
  , viewRoute = ("default","index2")
  , viewTemplate = Nothing
  }


nosuchaction :: Report -> Report
nosuchaction report =
  report
  { status = ok200
  , viewRoute = ("default","nosuchaction")
  }

inner :: Report -> Report 
inner report =
  report
  { status = ok200
  , viewRoute = ("default","inner")
  , subReports = [("insideinnerkey",report {actRoute = ("default","insideinner")} )]
  }

insideinner :: Report->Report
insideinner report = 
  report
  { status = ok200
  , viewRoute = ("default","insideinner")
  }
