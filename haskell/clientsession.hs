{-# LANGUAGE OverloadedStrings #-}

import Web.ClientSession
import Data.ByteString.Char8 as B

main = do
  (b,k) <- randomKey
  B.putStrLn b
  e <- encryptIO k "abc"
  return $ decrypt k e
 
