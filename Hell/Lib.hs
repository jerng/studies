{-# LANGUAGE OverloadedStrings #-}

module Hell.Lib 
  ( lookupBsonVal
  , addDebug
  , (?>>)
  , (<<?)
  , debugf
  , debugfps
  , redirectTo
  , (-->)
  )  where
import Data.Bson as Bson (Document, Field(..), Val,Label,cast')
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text as T 
import Hell.Attributed
import Hell.Conf 
import Hell.Parse.Forms
import Hell.Show
import Hell.Types
import Network.HTTP.Types.Header ( hLocation ) 
import Network.HTTP.Types (found302)

lookupBsonVal :: Val a => Label -> Document -> Maybe a
lookupBsonVal _key [] = Nothing
lookupBsonVal  key (field:exhead)
  | key == (label field) =  (cast' =<< Just (value field))
  | otherwise = lookupBsonVal key exhead

-- | In views, it's too late to update the (Report), so just format and print.
debugf :: T.Text -> T.Text
debugf a = T.concat 
  [ "<pre class=\"debug\"><b>debug: </b> <span>" , a , "</span></pre>" ]

debugfps :: Show a => a -> T.Text
debugfps a = debugf.T.pack.show $ a

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

redirectTo :: Report -> BS.ByteString -> Report
redirectTo rep loc = rep { status = found302, resHeaders = [(hLocation,loc)] }

(-->) :: Report -> BS.ByteString -> Report
(-->) = redirectTo
infix 1 -->
