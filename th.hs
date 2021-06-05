{-# LANGUAGE TemplateHaskell #-}

import Thlib

main :: IO [Char]
main = return $(hello)
