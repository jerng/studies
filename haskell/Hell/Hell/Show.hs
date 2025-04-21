{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hell.Show 
  ( showRequest
  , sinkForRequestBody
  , showDoc
  ) where 

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT,ResourceT)
import Data.Bson as Bson
import qualified Data.ByteString.Char8 as BS (ByteString)
import qualified Data.ByteString.UTF8 as BS.UTF8 (toString)
import Data.Conduit (Sink,yield,await,($$))
import Data.Conduit.Binary (sourceLbs)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Vault as V (Vault(..))
import Data.Word (Word8)
import Hell.Conf
import Hell.Types
import Network.HTTP.Types.Header ( Header ) 
import Network.Wai (Request(..),RequestBodyLength(..))

-- | extract the request body before calling this function,
-- using the commented-out method, so that it can be reused, variously.
showRequest :: BS.ByteString -> ResourceT IO Request -> ResourceT IO String
showRequest reqBod resIOreq = do
  req <- lift.runResourceT $ resIOreq
  let requestMethod'' = show.requestMethod $ req
      httpVersion'' = show.httpVersion $ req
      rawPathInfo'' = show.rawPathInfo $ req
      rawQueryString'' = show.rawQueryString $ req
      serverName'' = show.serverName $ req
      serverPort'' = show.serverPort $ req
      requestHeaders'' = 
        intercalate "\n    " $ "" : (map showHeader $ requestHeaders req)
      isSecure'' = show.isSecure $ req
      remoteHost'' = show.remoteHost $ req
      pathInfo'' = intercalate "\n    " $ "" : (map show $ pathInfo req)
      queryString'' = intercalate "\n    " $ "" : (map show $ queryString req)

--  requestBody'' 
--    <- lift.runResourceT.showResIO $ (requestBody req $$ sinkForRequestBody)
  let vault'' = show.vault $ req
      requestBody'' = BS.UTF8.toString reqBod
      requestBodyLength'' = show.requestBodyLength $ req

  -- This is so not-optimised :P
  lift.return.concat $
    [ "Request {"
    , if Hell.Conf.appMode > FullAutoDebug then " <b>(set appMode ==\
      \ FullAutoDebug to see more fields)</b>" else "", "\n  "
    , if Hell.Conf.appMode > FullAutoDebug then "" else
      concat ["<b>vault</b> = ", vault'', ", \n\n  "]

    , "<b>remoteHost</b> = ", remoteHost'', ", \n  "
    , "<b>isSecure</b> = ", isSecure'', ","
    , if Hell.Conf.appMode > FullAutoDebug then "" else "\n", "\n  "  

    , "<b>serverName</b> = ", serverName'', ", \n  " 
    , "<b>serverPort</b> = ", serverPort'' , ","
    , if Hell.Conf.appMode > FullAutoDebug then "" else "\n", "\n  "  

    , "<b>rawPathInfo</b> = ", rawPathInfo'', ", \n  "
    , if Hell.Conf.appMode > FullAutoDebug then "" else
      concat ["<b>pathInfo</b> = ", pathInfo'', ", \n\n  "]

    , "<b>rawQueryString</b> = ", rawQueryString'', ", \n  "
    , if Hell.Conf.appMode > FullAutoDebug then "" else
      concat ["<b>queryString</b> = ", queryString'', ", \n\n  "]

    , "<b>httpVersion</b> = ", httpVersion'', ", \n  "
    , "<b>requestMethod</b> = ", requestMethod'', ", \n  "
    , "<b>requestHeaders</b> = ", requestHeaders'' , ","
    , if Hell.Conf.appMode > FullAutoDebug then "" else "\n", "\n  "  

    , if Hell.Conf.appMode > FullAutoDebug then "" else
      concat ["<b>requestBodyLength</b> = ", requestBodyLength'', ", \n  "]

    , "<b>requestBody</b> = ", requestBody'', " \n"
    , "}"
    ]

sinkForRequestBody :: Sink BS.ByteString (ResourceT IO) BS.ByteString
sinkForRequestBody = do
  a :: (Maybe BS.ByteString) <- await
  case a of
    Nothing -> return ""
    Just b -> return b

showResIO :: (Show a) => ResourceT IO a -> ResourceT IO String
showResIO a = do
  b <- lift $ runResourceT a
  lift $ return $ show b

showHeader :: Header -> String
showHeader (headerName,headerValue) =
  concat  [ show headerName , " : " , show headerValue ]

-- TODO: At least, we should be able to print Vault keys $ 
instance Show Vault where
  show _ = "a :: Vault"

instance Show RequestBodyLength where
  show r = case r of 
    ChunkedBody -> "ChunkedBody"
    KnownLength w64 -> "KnownLength " ++ show w64

--------------------------------------------------------------------------------
-- Could be rewritten to be somewhat more Haskellian. Consider.
--
-- LIBRARY : PRETTY PRINT FOR Data.Bson's types
-- (The following code remains inconsistent, and much improvable.)
-- ind : indentation levels 
-- doc : documents
-- arr : arrays
-- fld : fields
-- val : values
-- col : collection

-- Indentation token could be moved to Hell.Conf
showInd :: Int -> T.Text
showInd ind = T.replicate ind "    "

showDoc :: Bool -> Int -> Bson.Document -> T.Text
showDoc types ind doc = T.concat $ (T.concat ["\n", showInd ind,"["])
  : T.intercalate (T.concat ["\n",showInd ind,","] ) 
    (map (showFld types (ind+1)) doc)
  : "\n"
  : showInd ind
  : "]<span class=\"type-signature\"> :: [Document]</span>"
  : []  

showArr :: Bool->Int -> [Bson.Value] -> T.Text
showArr types ind arr = T.concat $ (T.concat ["\n", showInd ind,"["])
  : T.intercalate (T.concat ["\n",showInd ind,","] )
    (map (\v -> T.concat [ showInd 1, showVal types (ind+1) v ]) arr)
  : "\n"
  : showInd ind
  : "]"
  : []

showFld :: Bool->Int -> Bson.Field -> T.Text
showFld types ind fld@((Bson.:=) {Bson.label=l, Bson.value=v }) = T.concat $ (T.tail $ showInd 1)
  : T.pack(show l)
  : " =: "
  : showVal types ind v
  : []

showVal :: Bool -> Int -> Bson.Value -> T.Text
showVal types ind val = case val of 
  Float v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Float Double</span>"
      else  ""
    ]
  String v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: String T.Text</span>"
      else  ""
    ]
  Doc v -> showDoc types ind v
  Array v -> T.concat 
    [ showArr types ind v
    , if    types 
      then  "<span class=\"type-signature\"> :: Array [Bson.Value]</span>"
      else  ""
    ]
  Bin v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Bin Binary</span>"
      else  ""
    ]
  Fun v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Fun Function</span>"
      else  ""
    ]
  Uuid v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Uuid UUID</span>"
      else  ""
    ]
  Md5 v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Md5 MD5</span>"
      else  ""
    ]
  UserDef v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: UserDef UserDefined</span>"
      else  ""
    ]
  ObjId v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: ObjId ObjectId</span>"
      else  ""
    ]
  Bool v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Bool Bool</span>"
      else  ""
    ]
  UTC v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: UTC UTCTime</span>"
      else  ""
    ]
  Null -> T.concat 
    [ "Null"
    , if    types 
      then  "<span class=\"type-signature\"> :: Null</span>"
      else  ""
    ]
  RegEx v -> T.concat 
    [ T.pack(show v) 
    , if    types 
      then  "<span class=\"type-signature\"> :: RegEx Regex</span>"
      else  ""
    ]
  JavaScr v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: JavaScr Javascript</span>"
      else  ""
    ]
  Sym v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Sym Symbol</span>"
      else  ""
    ]
  Int32 v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Int32 Int32</span>"
      else  ""
    ]
  Int64 v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Int64 Int64</span>"
      else  ""
    ]
  Stamp v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: Stamp MongoStamp</span>"
      else  ""
    ]
  MinMax v -> T.concat 
    [ T.pack(show v)
    , if    types 
      then  "<span class=\"type-signature\"> :: MinMax MinMaxKey</span>"
      else  ""
    ]

--------------------------------------------------------------------------------
