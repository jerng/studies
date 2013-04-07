subtest1 :: Report -> Report
subtest1 report = do 
  -- let x = Controllers.Default.inner report
  report  
    { status = ok200
    , viewRoute = ("test","subtest1") 
    }

subtest2 :: Report -> Report
subtest2 report = 
  report
  { status = ok200
  , viewRoute = ("test","subtest2") 
  }
