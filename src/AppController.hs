{-  This example AppController does absolutely nothing.
  Copy it to ./src/AppController.hs, and modify it to your liking.
-}
{-# LANGUAGE OverloadedStrings #-}

module AppController where

import Hell.Lib

-- | Hell.Server.applyActionToReport calls this.
main :: Report -> Report
main rep = 
  let r = action rep $ rep
  in  r <<? ( tAppend "Request {data_}:" $ showDoc True 0 $ data_ r )
        <<? ( tAppend "Request {params}:" $ tPack.show $ params r ) 
        <<? ( tAppend "Request {files}:" $ tPack.show $ files r ) 

-- | Hell.Server.applyActionToSubReport calls this.
subMain :: Report -> Report
subMain rep = action rep $ rep
