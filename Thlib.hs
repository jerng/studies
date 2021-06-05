{-# LANGUAGE TemplateHaskell #-}
module Thlib where

import Language.Haskell.TH

hello :: Q Exp
hello = [|"hello"|]

--hello = [|(1+1)|]

