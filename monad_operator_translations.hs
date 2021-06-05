main = 


{-

do let x = "something"

-- is equivalent to

do x <- return something

--

return :: Monad m => a -> m a  
This marks pure data "a" and makes it accessible to monad "m".
--
(a <- Monad m )
This marks data from monad "m" as pure, passes it to: a closure  
containing all remaining lines in the "do" block;

-}


--{- First equivalent block
--
  putStrLn "Greetings! What is your name?" >>
  getLine >>=
  ( \inpStr -> 
    getLine >>=
    ( \inpStr2 ->
      putStrLn $ "Welcome to Haskell, " ++ inpStr ++ " " ++ inpStr2 ++ "!"
    )
  )
--}

{- Second equivalent block
--
  do
  putStrLn "Greetings! What is your name?"
  inpStr <- getLine
  inpStr2 <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ " " ++ inpStr2 ++ "!"
--}

