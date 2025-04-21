{-# LANGUAGE OverloadedStrings #-}

module Hell.Debug where

import qualified Data.Text as T
import Hell.Conf

-- | In views, it's too late to update the (Report), so just format and print.
debugf :: T.Text -> T.Text
debugf a = T.concat 
  [ "<pre class=\"debug\"><b>debug: </b> <span>" , a , "</span></pre>" ]

debugMissingViewData :: T.Text -> T.Text
debugMissingViewData t  = if    appMode == Production
                          then  ""
                          else  Hell.Debug.debugf t

-- ***************************************************************************
-- | Updates the (debug) field of a report
addDebug :: T.Text -> Report -> Report
addDebug text rep = if    Hell.Conf.appMode == Production
                    then  rep
                    else  rep { debug = text : debug rep }

(?>>) :: T.Text -> Report -> Report 
infixr 2 ?>>
(?>>) = addDebug

(<<?) :: Report -> T.Text -> Report 
infixl 1 <<?
(<<?) = flip addDebug
-- ***************************************************************************
