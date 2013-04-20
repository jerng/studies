redirect :: Action 
redirect rep =  rep --> "/default/index" 

form :: Action
form rep = rep
  { status = ok200
  } 
  <<? showDoc True 0 ... form_ rep
  <<? tPack.show ... postQuery rep

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
