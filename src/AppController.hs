{-  This example AppController does absolutely nothing.
  Copy it to ./src/AppController.hs, and modify it to your liking.
-}
{-# LANGUAGE OverloadedStrings #-}

module AppController where

import qualified Data.Text as T
import Hell.Lib

-- | Hell.Server.applyActionToReport calls this.
main :: Report -> Report
main rep = 
  let r = action rep $ rep
  in  r <<? tAppend "Request {bson}:" ... showDoc True 0 ... data_ r    

-- | Hell.Server.applyActionToSubReport calls this.
subMain :: Report -> Report
subMain rep = action rep $ rep
