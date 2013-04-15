{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hell.Lib (
  
  -- | Defined in Blaze.ByteString.Builder.Char.Utf8:
    fromText

  -- | Defined in Control.Monad:
  , foldM
  --, liftM

  -- | Defined in Control.Monad.Trans.Class:
  , lift
  
  -- | Defined in Data.Bson:
  , (Bson.!?)
  , Bson.look
  --, bsonLookup {-is a synonym -}
  , Bson.valueAt
  , Bson.at
  , Bson.include
  , Bson.exclude
  , Bson.merge
  , (Bson.=:)
  , (Bson.=?)
  , Bson.fval
  , Bson.cast
  , Bson.typed
  , Bson.typeOfVal
  , Bson.timestamp
  , Bson.genObjectId

  -- | Defined in Data.ByteString:
  --, bsSplit 
  , bsAppend
  , bsEmpty
  , bsConcat
  --, bsFindIndex
  --, bsSplitAt
  , bsTail
  , bsSpan
  , bsTakeWhile

  -- | Defined in Data.ByteString.Lazy:

  -- | Defined in Data.List:
  , nub

  -- | Defined in Data.List.Utils:
  , hasKeyAL
  , keysAL 

  -- | Defined in Data.Maybe:
  , fromMaybe
  , isJust

  -- | Defined in Data.String.Utils:
  , replace

  -- | Defined in Data.Text:
  , tConcat
  , tPack
  , tUnpack
  , tToLower
  , tIntercalate
  , tLines
  , tUnlines
  , tSplitOn
  , tReplace
  , tAppend
  , tTake
  , tWords
  , tReadFile
  , tWriteFile
  , tPutStrLn
  , tReplicate

  -- | Defined in Data.Text.Encoding:
  , decodeUtf8

  -- | Defined in Hell.Conf:
  , defaultSession
  , undecryptableSession
  , useCookies
  , useSessions
  , useEncryption
  , keyOfTemplatedView
  , keyOfMetaView
  , metaNoSuchAction
  , warpServer 
  , appMode
  , defaultReport
  , defaultCookie
  , sessionCookieName
  , defaultHeaders
  , defaultStatus
  , defaultViewTemplate
  , defaultCookieName
  , indexAction
  , defaultRoute
  , missingActionRoute
  , missingViewRoute
  , staticFileRoute

  , templatedResources
  , staticResources
  , sliceIDsOf
  , fromPath
  , toPath
  , controllers
  , models
  , views
  , messageStartMakeHell
  , messageStartTryHell
  , messageJobDone
  , scriptExtension
  , viewExtension
  , viewBsonHelpers
  , ViewExpression

  , toText

  -- | Defined in Hell.Types:
  , IV
  , Key
  , ResourceT
  , Binary
  , ByteString
  , LByteString

  , Document
  , Field(..)
  , Label
  , Value(..)
  , Val(..)
  , BsonBinary(..){- is a synonym -}
  , Function(..)
  , UUID(..)
  , MD5(..)
  , UserDefined(..)
  , Regex(..)
  , Javascript(..)
  , Symbol(..)
  , MongoStamp(..)
  , MinMaxKey(..)
  , ObjectId(..)
  , Map
  , Text
  , Request (..)
  , Response (..)
  , Status (..)
  , Header
  , accepted202
  , ok200
  , found302
  , Settings

  , ReportHandler
  , Action
  , ResourceNameText
  , ControllerName
  , ActionName
  , Route

  , ReportM

  , Report (..)
  , ResourceName (..)
  , SliceTag (..)
  , Slice (..)
  , Unrendered (..)
  , AppMode (..)

  , CookieAttribute
  , CookieValue
  , CookieAVPair
  , Cookie (..)
  
  -- | Defined in Network.HTTP.Types.Header
  --, hCookie
  , hLocation

  -- | Defined in Web.ClientSession 
  , randomIV 
  , encrypt
  , encryptIO
  , decrypt
  , getDefaultKey

  -- | Defined below:
--  , lookupViewDictionary
  , cookieHeadersToKVs
  , cookieToHeader
  , onlyCookieHeaders
  , decdoc
  , encdoc
  , lookupBsonVal
  , partitionM
  , showRequest
  , addDebug
  , (?>>)
  , (<<?)
  , (...)
  , showDoc
  , debugf
  , debugfps
  , redirectTo
  , (-->)
)  where

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Monad (foldM{-, liftM-})
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified 
       Data.Bson as Bson
import Data.Bson.Binary (putDocument, getDocument)
import qualified 
       Data.ByteString.Char8 as BS (concat, intercalate, empty, tail, span,
       takeWhile,split,append)
import qualified
       Data.ByteString.Lazy.Char8 as LBS (toChunks,fromChunks,split,span,tail)
import qualified Data.ByteString.Search as BSS (replace)
import Data.ByteString.Search.Substitution
import Data.Conduit ({-Source,-}Sink,yield,await,($$))
import Data.Dynamic (fromDyn, fromDynamic, toDyn)
import Data.List (nub,intercalate)
import Data.List.Utils (hasKeyAL, keysAL)
import Data.Maybe (fromMaybe,isJust)
import Data.String.Utils (replace)
import qualified 
       Data.Text as T (concat, pack, toLower, intercalate, lines, unlines,
       splitOn, replace, append, take, words, unpack, replicate )
import qualified 
       Data.Text.IO as T (readFile,writeFile,putStrLn)
import Data.Text.Encoding (decodeUtf8{-, encodeUtf8-})
import Hell.Conf 
import Hell.Types
import Network.HTTP.Types.Header ( hCookie, hLocation ) 
-- import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.ClientSession (randomIV, encrypt, encryptIO, decrypt, getDefaultKey)

bsAppend :: ByteString -> ByteString -> ByteString
bsAppend = BS.append

lbsSplit :: Char -> LByteString -> [LByteString]
lbsSplit = LBS.split

bsSplit :: Char -> ByteString -> [ByteString]
bsSplit = BS.split

bssReplace :: Substitution rep => ByteString -> rep -> ByteString -> LByteString
bssReplace = BSS.replace

--bssSplit :: ByteString -> ByteString -> [ByteString]
--bssSplit = BSS.split

bsSpan :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
bsSpan = BS.span

lbsSpan :: (Char -> Bool) -> LByteString -> (LByteString, LByteString)
lbsSpan = LBS.span

bsEmpty :: ByteString
bsEmpty = BS.empty

bsConcat :: [ByteString] -> ByteString
bsConcat = BS.concat

lbsTail :: LByteString -> LByteString
lbsTail = LBS.tail

bsTail :: ByteString -> ByteString
bsTail = BS.tail

--bsFindIndex :: (Char -> Bool) -> ByteString -> Maybe Int
--bsFindIndex = BS.findIndex

--bsSplitAt :: Int -> ByteString -> (ByteString, ByteString)
--bsSplitAt = BS.splitAt

bsTakeWhile :: (Char -> Bool) -> ByteString -> ByteString
bsTakeWhile = BS.takeWhile

tConcat :: [Text] -> Text
tConcat = T.concat

tToLower :: Text -> Text
tToLower = T.toLower

tPack :: String -> Text
tPack = T.pack

tUnpack :: Text -> String
tUnpack = T.unpack

tIntercalate :: Text -> [Text] -> Text
tIntercalate = T.intercalate

tLines :: Text -> [Text]
tLines = T.lines

tUnlines :: [Text] -> Text
tUnlines = T.unlines

tSplitOn :: Text -> Text -> [Text]
tSplitOn = T.splitOn

tReplace :: Text -> Text -> Text -> Text
tReplace = T.replace

tAppend :: Text -> Text -> Text
tAppend = T.append

tTake :: Int -> Text -> Text
tTake = T.take

tWords :: Text -> [Text]
tWords = T.words

tReadFile :: FilePath -> IO Text
tReadFile = T.readFile

tWriteFile :: FilePath -> Text -> IO ()
tWriteFile = T.writeFile

tPutStrLn :: Text -> IO()
tPutStrLn = T.putStrLn

tReplicate :: Int -> Text -> Text
tReplicate = T.replicate

onlyCookieHeaders :: [Header] -> [Header]
onlyCookieHeaders headers = filter ((hCookie==).fst) headers

cookieToHeader :: Cookie -> Header
cookieToHeader c = ("Set-Cookie", cookieToBS c)

-- | Perhaps the entire Cookie type should be refactored with (Maybe)
cookieToBS :: Cookie -> ByteString
cookieToBS c = BS.intercalate "; " $ concat
  [ [ BS.concat [ cookieName c, "=", cookieValue c ] ]
  , if cookieHttpOnly c then ["HttpOnly"] else []
  , if cookieSecure c then ["Secure"] else []
  , map cookieAVPairToBS $ cookiePairs c
  ]

-- | Perhaps, introduce a type synonym for (ByteString,ByteString)
cookieHeadersToKVs :: [Header] -> [(ByteString,ByteString)]
cookieHeadersToKVs hs = concatMap headerValueToKVs hs

headerValueToKVs :: Header -> [(ByteString,ByteString)]
headerValueToKVs (_,bs) = 
    map 
    ( \lbs->  
        case lbsSpan (/='=') lbs of
          ("",_)  -> ("","")
          (k,"")  -> (bsConcat.LBS.toChunks...k,"")
          (k,v)   -> (bsConcat.LBS.toChunks...k, bsConcat.LBS.toChunks.lbsTail...v)
    ) $ 
    lbsSplit ';' $ bssReplace " " (""::ByteString) bs

cookieAVPairToBS :: CookieAVPair -> ByteString
cookieAVPairToBS (attr,val) = BS.concat [attr,"=",val] 

encdoc :: Document -> ByteString
encdoc doc = BS.concat.LBS.toChunks.runPut...putDocument doc 

decdoc :: ByteString -> Document
decdoc bin = runGet getDocument $ LBS.fromChunks [ bin ]

lookupBsonVal :: Val a => Label -> Document -> Maybe a
lookupBsonVal _key [] = Nothing
lookupBsonVal  key (field:exhead)
  | key == (label field) =  (cast' =<< Just (value field))
  | otherwise = lookupBsonVal key exhead

-- ****************************************************************************
-- | FROM: http://stackoverflow.com/questions/15216621
--    /can-partition-be-applied-to-a-io-bool
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs = foldM f ([], []) xs
  where 
    f (a, b) x = do
      flag <- p x
      return $ if flag 
        then (x : a, b) 
        else (a, x : b)
--
-- ****************************************************************************

showRequest :: ResourceT IO Request -> ResourceT IO String
showRequest resIOreq = do
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

  requestBody'' 
    <- lift.runResourceT.showResIO $ (requestBody req $$ sinkForRequestBody)
  let vault'' = show.vault $ req
      requestBodyLength'' = show.requestBodyLength $ req

  -- This is so not-optimised :P
  lift.return.concat $
    [ "Request {"
    , if Hell.Conf.appMode > Development0 then " <b>(set appMode ==\
      \ Development0 to see more fields)</b>" else "", "\n  "
    , if Hell.Conf.appMode > Development0 then "" else
      concat ["<b>vault</b> = ", vault'', ", \n\n  "]

    , "<b>remoteHost</b> = ", remoteHost'', ", \n  "
    , "<b>isSecure</b> = ", isSecure'', ","
    , if Hell.Conf.appMode > Development0 then "" else "\n", "\n  "  

    , "<b>serverName</b> = ", serverName'', ", \n  " 
    , "<b>serverPort</b> = ", serverPort'' , ","
    , if Hell.Conf.appMode > Development0 then "" else "\n", "\n  "  

    , "<b>rawPathInfo</b> = ", rawPathInfo'', ", \n  "
    , if Hell.Conf.appMode > Development0 then "" else
      concat ["<b>pathInfo</b> = ", pathInfo'', ", \n\n  "]

    , "<b>rawQueryString</b> = ", rawQueryString'', ", \n  "
    , if Hell.Conf.appMode > Development0 then "" else
      concat ["<b>queryString</b> = ", queryString'', ", \n\n  "]

    , "<b>httpVersion</b> = ", httpVersion'', ", \n  "
    , "<b>requestMethod</b> = ", requestMethod'', ", \n  "
    , "<b>requestHeaders</b> = ", requestHeaders'' , ","
    , if Hell.Conf.appMode > Development0 then "" else "\n", "\n  "  

    , if Hell.Conf.appMode > Development0 then "" else
      concat ["<b>requestBodyLength</b> = ", requestBodyLength'', ", \n  "]

    , "<b>requestBody</b> = ", requestBody'', " \n"
    , "}"
    ]

sinkForRequestBody :: Sink ByteString (ResourceT IO) ByteString
sinkForRequestBody = do
  a :: (Maybe ByteString) <- await
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

instance Show Vault where
  show _ = "a :: Vault"

instance Show RequestBodyLength where
  show r = case r of 
    ChunkedBody -> "ChunkedBody"
    KnownLength w64 -> "KnownLength " ++ show w64

-- | In views, it's too late to update the (Report), so just format and print.
debugf :: Text -> Text
debugf a = tConcat 
  [ "<pre class=\"debug\"><b>debug: </b> <span>" , a , "</span></pre>" ]

debugfps :: Show a => a -> Text
debugfps a = debugf.tPack.show...a

-- | Updates the (debug) field of a report
addDebug :: Text -> Report -> Report
addDebug text rep =  if    Hell.Conf.appMode == Production
                        then  rep
                        else  rep { debug = text : debug rep }

(?>>) :: Text -> Report -> Report 
infixr 2 ?>>
(?>>) = addDebug

(<<?) :: Report -> Text -> Report 
infixl 1 <<?
(<<?) = flip addDebug

-- | Invented for use where it is less noisy to use (a.b.c...d)
-- instead of (a.b.c $ d) 
--
-- Perhaps to be avoided where it may be too subtle to use (a b... c.d.e... f g)
-- instead of (a b $ c.d.e $ f g)
f ... a = f a
infixr 8 ...

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
showBson :: [Document] -> Text
showBson docList = tIntercalate "\n" (map (showDoc 0) docList)

-- Indentation token could be moved to Hell.Conf
showInd :: Int -> Text
showInd ind = tReplicate ind "  "

showDoc :: Int -> Document -> Text
showDoc ind doc = tConcat $! "[\n"
  : tIntercalate ",\n" (map (showFld (ind+1)) doc)
  : "\n"
  : showInd ind
  : "]\n"
  : ( showInd ind )
  : []  

showArr :: Int -> [Value] -> Text
showArr ind arr = tConcat $! "[\n"
  : tIntercalate ",\n"
    (map (\v -> tConcat [ showInd (ind+1), showVal (ind+1) v ]) arr)
  : "\n"
  : showInd ind
  : "]\n"
  : showInd ind 
  : []

showFld :: Int -> Field -> Text
showFld ind fld@((:=) {label=l, value=v }) = tConcat $! showInd ind
  : T.pack(show l)
  : " =: "
  : showVal ind v
  : []

showVal :: Int -> Value -> Text
showVal ind val = case val of 
  Float v     -> tConcat [T.pack(show v)," (Float Double)"]
  String v    -> tConcat [T.pack(show v)," (String Text)"]
  Doc v       -> tConcat [showDoc ind v,"(Doc Document)"]
  Array v     -> tConcat [showArr ind v,"(Array [Value])"]
  Bin v       -> tConcat [T.pack(show v)," (Bin Binary)"]
  Fun v       -> tConcat [T.pack(show v)," (Fun Function)"]
  Uuid v      -> tConcat [T.pack(show v)," (Uuid UUID)"]
  Md5 v       -> tConcat [T.pack(show v)," (Md5 MD5)"]
  UserDef v   -> tConcat [T.pack(show v)," (UserDef UserDefined)"]
  ObjId v     -> tConcat [T.pack(show v)," (ObjId ObjectId)"]
  UTC v       -> tConcat [T.pack(show v)," (UTC UTCTime)"]
  Null        ->                          "Null (Null)"
  RegEx v     -> tConcat [T.pack(show v)," (RegEx Regex)"]
  JavaScr v   -> tConcat [T.pack(show v)," (JavaScr Javascript)"]
  Sym v       -> tConcat [T.pack(show v)," (Sym Symbol)"]
  Int32 v     -> tConcat [T.pack(show v)," (Int32 Int32)"]
  Int64 v     -> tConcat [T.pack(show v)," (Int64 Int64)"]
  Stamp v     -> tConcat [T.pack(show v)," (Stamp MongoStamp)"]
  MinMax v    -> tConcat [T.pack(show v)," (MinMax MinMaxKey)"]

-- | Collections; maybe later.
-- Data.MongoDB.Query.Collection 
--showCols :: [Collection] -> Text
--showCols cols = concat $! "[\n\t"
--  : tIntercalate ",\n\t" cols
--  : "\n]"
--  : []

--------------------------------------------------------------------------------

redirectTo :: Report -> ByteString -> Report
redirectTo rep loc = rep { status = found302, resHeaders = [(hLocation,loc)] }

(-->) :: Report -> ByteString -> Report
(-->) = redirectTo
