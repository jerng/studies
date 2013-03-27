{-  This example AppController does absolutely nothing.
  Copy it to ./src/AppController.hs, and modify it to your liking.
-}
{-# LANGUAGE OverloadedStrings #-}

module AppController where

import qualified Data.Text as T
import Hell.Lib

-- | Hell.Server.applyActionToReport calls this.
main :: Report -> (Report -> Report) -> Report
main report action = action report

-- | Hell.Server.applyActionToSubReport calls this.
subMain :: Report -> (Report -> Report) -> Report
subMain report action = action report
