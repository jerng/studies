index :: Report -> Report
index rep = 
  "more debuggable stuff (because if you do, you have to type from right to left)" ?>> 
  "debuggable stuff (I don't recommend you use the (?>>) operator)" ?>> 

  rep
  { status = ok200
  , viewRoute = ("default","index")
  , viewBson = 
    [ "someInt" :=Int32 999
    , "someFloat" :=Float 3.132
    , "someText" :=String "I_AM_TEXT"
    , "someIntList" := Array [Int32 1,Int32 2, Int32 3]
    ]
  , subReports = [("innerkey", rep { actRoute = ("default", "inner") })] 
  , viewTemplate = Just ("default","template")
  , resCookies = defaultCookie 
    { cookieName = "newCookie"
    , cookieValue = "newValue"
    } : resCookies rep
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
index2 rep = 
  rep
  { status = ok200
  , viewRoute = ("default","index2")
  , viewTemplate = Nothing
  }


nosuchaction :: Report -> Report
nosuchaction rep =
  rep
  { status = ok200
  , viewRoute = ("default","nosuchaction")
  }

inner :: Report -> Report 
inner rep =
  rep
  { status = ok200
  , viewRoute = ("default","inner")
  , subReports = [("insideinnerkey",rep {actRoute = ("default","insideinner")} )]
  }

insideinner :: Report->Report
insideinner rep = 
  rep
  { status = ok200
  , viewRoute = ("default","insideinner")
  }
