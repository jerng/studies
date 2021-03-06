{-  This example AppController does absolutely nothing.

  Copy it to ./src/AppController.hs, and modify it to your liking.
-}
{-# LANGUAGE OverloadedStrings #-}

module AppController where

import Hell.Lib
import qualified Data.Text as T (Text,pack,append,concat)

-- | Hell.Server.applyActionToReport calls this.
main :: Report -> Report
main rep = 
  let r = action rep $ rep
  in  r <<? ( T.append "Request {data_}:" $ showDoc True 0 $ data_ r )

-- | Hell.Server.applyActionToSubReport calls this.
subMain :: Report -> Report
subMain rep = action rep $ rep
