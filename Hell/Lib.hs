{- This is where all the miscellaneous functions go. -}
{-# LANGUAGE OverloadedStrings #-}

module Hell.Lib 
  ( module Hell.Conf
  , module Hell.Debug
  , module Hell.Show
  , redirectTo
  , (-->)
  , url
  )  where
import Data.Bson as Bson (Document, Field(..), Val,Label,cast')
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Text as T 
import Hell.Attributed
import Hell.Conf 
import Hell.Debug
import Hell.Parse.Forms
import Hell.Show
import Network.HTTP.Types.Header ( hLocation ) 
import Network.HTTP.Types (found302)


-- **** VIEWS: 

redirectTo :: Report -> BS.ByteString -> Report
redirectTo rep loc = rep { status = found302, resHeaders = [(hLocation,loc)] }

(-->) :: Report -> BS.ByteString -> Report
(-->) = redirectTo
infix 1 -->

url :: Route -> T.Text -> T.Text
url (c,a) afterA = T.concat
  [ if    Hell.Conf.appWebRoot == ""
    then  ""
    else  "/"
  , Hell.Conf.appWebRoot
  , "/", c
  , "/", a
  , "/"
  , afterA
  ]
