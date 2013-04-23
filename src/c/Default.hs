import Data.Bson (Document, Field (..), Value (..))
import qualified Data.Text as T
import Network.HTTP.Types (ok200)

index :: Report -> Report
index rep = 
  "more debuggable stuff (because if you do, you have to type from right to left)" ?>> 
  "debuggable stuff (I don't recommend you use the (?>>) operator)" ?>> 

  rep
  { status = ok200
  , viewBson = 
    [ "someInt" :=Int32 999
    , "someFloat" :=Float 3.132
    , "someText" :=String "I_AM_TEXT"
    , "someIntList" := Array [Int32 1,Int32 2, Int32 3]
    ]
  , subReports = [("innerkey", rep { actRoute = ("default", "inner") })] 
  , viewTemplateRoute = Just ("default","template")
  , resCookies = defaultCookie 
    { cookieName = "newCookie"
    , cookieValue = "newValue"
    } : resCookies rep
  }

      <<? "even more debuggable stuff"
      <<? "lastly, for good measure" 
      <<? "default.hs line 14" 
      <<? ( T.pack.show $ 666 )
      <<? "just trying to debug an int"
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
