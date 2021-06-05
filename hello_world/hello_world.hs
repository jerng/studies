{-# LANGUAGE OverloadedStrings #-}
-- this handles bytestrings

import qualified Data.Text    as T
import qualified Data.Text.IO as T
-- these handle UTF-8

main = T.putStrLn " ps... hello world."
--main = T.putStrLn "čušpajž日本語; ps... hello world."
-- does not seem to work on windows 7, ghc 7.4.1
