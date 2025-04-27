index :: Report -> Report
index rep =
  let   -- A bunch of declarations:
    a = 1 
  in    -- A single expression:
    rep
    { status = ok200
    , modelArgs = 
      [ "someText" := Doc 
          [ "actionType" := String "findAll"
          , "selector" := Doc []
          , "collection" := String "testcollection"
          ]  
      ]
    , viewData_ = 
      [ "someInt" :=Int32 999
      , "someFloat" :=Float 3.132

      , "someText" :=String (debugf "I_AM_TEXT")
        -- replace this

      , "someIntList" := Array [Int32 1,Int32 2, Int32 3]
      ]
    , subReports = [("innerkey", rep { actRoute = ("default", "inner") })] 
    , viewTemplateRoute = Just ("default","template")
    , resCookies = defaultCookie 
      { cookieName = "newCookie"
      , cookieValue = "newValue"
      } : resCookies rep
    }
    --  TODO :Try to make a helper function for sending variables to the
    --  ViewableList.
    -- TODO: have the rendered create a default viewRoute if it's not specified

index2 :: Report -> Report
index2 rep = 
  rep
  { status = ok200
  , viewTemplateRoute = Nothing
  }


nosuchaction :: Report -> Report
nosuchaction rep =
  rep
  { status = ok200
  }

inner :: Report -> Report 
inner rep =
  rep
  { status = ok200
  , subReports = [("insideinnerkey",rep {actRoute = ("default","insideinner")} )]
  }

insideinner :: Report->Report
insideinner rep = 
  rep
  { status = ok200
  }
