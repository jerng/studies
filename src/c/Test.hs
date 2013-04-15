redirect :: Action 
redirect rep = rep --> "/default/index" 

subtest1 :: Action 
subtest1 rep = do 
  rep  
    { status = ok200
    , viewRoute = ("test","subtest1") 
    }

subtest2 :: Action
subtest2 rep = 
  rep
  { status = ok200
  , viewRoute = ("test","subtest2") 
  }
