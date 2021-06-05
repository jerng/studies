{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}

import System.Cmd
import Text.Regex
import GHC.IO.Exception

main = speak =<< stringFromFile 

stringFromFile :: IO String
stringFromFile = readFile "input"

speak :: String -> IO ExitCode 
speak s = do
  let r = mkRegex "\""
  let t = subRegex r s "\\\"" 
  let command = "espeak -p 0 -s 160 \"" ++ t ++ "\""
  putStrLn command
  system command
