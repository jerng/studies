subtest1 :: Request -> AppControllerVars -> Report
subtest1 request appControllerVariables = 
  Report ok200 ("test","subtest1") []

subtest2 :: Request -> AppControllerVars -> Report
subtest2 request appControllerVariables = 
  Report ok200 ("test","subtest2") []
