{-# LANGUAGE OverloadedStrings #-}

module Hell.Parse.Headers 
  ( encdoc
  , decdoc
  , cookieHeadersToKVs
  , onlyCookieHeaders
  , cookieToHeader
  ) where

import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Bson (Document,Field(..))
import Data.Bson.Binary (putDocument, getDocument)
import qualified Data.ByteString.Char8 as BS (ByteString,intercalate,concat)
import qualified Data.ByteString.Lazy.Char8 as LBS (toChunks,fromChunks,split,span,tail)
import qualified Data.ByteString.Search as BS.Search (replace)
import Hell.Types
import Network.HTTP.Types.Header ( Header,hCookie ) 

onlyCookieHeaders :: [Header] -> [Header]
onlyCookieHeaders headers = filter ((hCookie==).fst) headers

cookieToHeader :: Cookie -> Header
cookieToHeader c = ("Set-Cookie", cookieToBS c)

-- | Perhaps the entire Cookie type should be refactored with (Maybe)
cookieToBS :: Cookie -> BS.ByteString
cookieToBS c = BS.intercalate "; " $ concat
  [ [ BS.concat [ cookieName c, "=", cookieValue c ] ]
  , if cookieHttpOnly c then ["HttpOnly"] else []
  , if cookieSecure c then ["Secure"] else []
  , map cookieAVPairToBS $ cookiePairs c
  ]

-- | Perhaps, introduce a type synonym for (ByteString,ByteString)
cookieHeadersToKVs :: [Header] -> [(BS.ByteString,BS.ByteString)]
cookieHeadersToKVs hs = concatMap headerValueToKVs hs

headerValueToKVs :: Header -> [(BS.ByteString,BS.ByteString)]
headerValueToKVs (_,bs) = 
  map 
  ( \lbs->  
      case LBS.span (/='=') lbs of
        ("",_)  -> ( "", "" )
        (k,"")  -> ( BS.concat.LBS.toChunks $ k, "" )
        (k,v)   -> ( BS.concat.LBS.toChunks $ k
                   , BS.concat.LBS.toChunks.LBS.tail $ v
                   ) -- where '=' is the head 
  ) $ 
  LBS.split ';' $ BS.Search.replace " " (""::BS.ByteString) bs

cookieAVPairToBS :: CookieAVPair -> BS.ByteString
cookieAVPairToBS (attr,val) = BS.concat [attr,"=",val] 

encdoc :: Document -> BS.ByteString
encdoc doc = BS.concat.LBS.toChunks.runPut $ putDocument doc 

decdoc :: BS.ByteString -> Document
decdoc bin = runGet getDocument $ LBS.fromChunks [ bin ]
