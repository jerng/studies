subtest1 :: Report -> Report
subtest1 report = 
  report  
  { status = ok200
  , routeV = ("test","subtest1") 
  }

subtest2 :: Report -> Report
subtest2 report = 
  report
  { status = ok200
  , routeV = ("test","subtest2") 
  }
