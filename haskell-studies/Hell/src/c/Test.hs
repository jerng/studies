import Network.HTTP.Types (ok200)

redirect :: Action 
redirect rep =  rep --> "/default/index" 

form :: Action
form rep = rep
  { status = ok200
  } 

form2 :: Action
form2 rep = rep
  { status = ok200
  } 

subtest1 :: Action 
subtest1 rep = do 
  rep  
    { status = ok200
    }

subtest2 :: Action
subtest2 rep = 
  rep
  { status = ok200
  }
