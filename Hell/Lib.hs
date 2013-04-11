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
  , fromJust
  , fromMaybe

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

  -- | Defined in Hell.Conf:
  , keyOfTemplatedView
  , keyOfMetaView
  , metaNoSuchAction
  , hellServerPort
  , appMode
  , defaultReport
  , defaultCookie
  , sessionCookieName
  , defaultHeaders
  , defaultStatus
  , defaultViewTemplate
  , indexAction
  , defaultRoute
  , noSuchActionRoute
  , noSuchViewRoute
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
  , ResourceT
  , Binary
  , ByteString

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
  , hCookie

  -- | Defined in Network.Wai.Handler.Warp
  , run

  -- | Defined in Web.ClientSession 
  , encryptIO
  , decrypt
  , getDefaultKey

  -- | Defined below:
--  , lookupViewDictionary
  , cookieToBS
  , onlyCookies
  , decdoc
  , encdoc
  , lookupBsonVal
  , partitionM
  , showRequest
  , addDebug
  , (?>>)
  , (<<?)
  , (...)

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
       takeWhile)
import Data.ByteString.Lazy (toChunks,fromChunks)
import Data.Conduit ({-Source,-}Sink,yield,await,($$))
import Data.Dynamic (fromDyn, fromDynamic, toDyn)
import Data.List (nub,intercalate)
import Data.List.Utils (hasKeyAL, keysAL)
import Data.Maybe (fromJust,fromMaybe)
import Data.String.Utils (replace)
import qualified 
       Data.Text as T (concat, pack, toLower, intercalate, lines, unlines,
       splitOn, replace, append, take, words, unpack )
import qualified 
       Data.Text.IO as T (readFile,writeFile,putStrLn)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hell.Conf 
import Hell.Types
import Network.HTTP.Types.Header ( hCookie ) 
import Network.Wai.Handler.Warp (run)
-- import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.ClientSession (encryptIO, decrypt, getDefaultKey)

--bsSplit :: Char -> ByteString -> [ByteString]
--bsSplit = BS.split

bsSpan :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
bsSpan = BS.span

bsEmpty :: ByteString
bsEmpty = BS.empty

bsConcat :: [ByteString] -> ByteString
bsConcat = BS.concat

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

onlyCookies :: [Header] -> [Header]
onlyCookies headers = filter ((hCookie==).fst) headers

-- | Perhaps the entire Cookie type should be refactored with (Maybe)
cookieToBS :: Cookie -> ByteString
cookieToBS c = BS.intercalate "; " $ concat
  [ [ BS.concat [ cookieName c, "=", cookieValue c ] ]
  , if cookieHttpOnly c then ["HttpOnly"] else []
  , if cookieSecure c then ["Secure"] else []
  , map cookieAVPairToBS $ cookiePairs c
  ]

cookieAVPairToBS :: CookieAVPair -> ByteString
cookieAVPairToBS (attr,val) = BS.concat [attr,"=",val] 

encdoc :: Document -> ByteString
encdoc doc = BS.concat $ toChunks $ runPut $ putDocument doc 

decdoc :: ByteString -> Document
decdoc bin = runGet getDocument $ fromChunks [ bin ]

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
    , if Hell.Conf.appMode > Development0 then " <b>(set appMode == Development0 to see more fields)</b>" else "", "\n  "
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

addDebug :: Text -> Report -> Report
addDebug text report = report { debug = text : debug report }

(?>>) :: Text -> Report -> Report 
infixr 2 ?>>
(?>>) = addDebug

(<<?) :: Report -> Text -> Report 
infixl 1 <<?
(<<?) = flip addDebug

f ... a = f a
infixr 8 ...
