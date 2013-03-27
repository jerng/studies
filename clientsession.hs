{-# LANGUAGE OverloadedStrings #-}

import Web.ClientSession
import Data.ByteString.Char8 as B

main = do
  k <- getDefaultKey
  e <- encryptIO k "abc"
  return $ decrypt k e
 
