{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hell.Lib (
  
  -- | Defined in Blaze.ByteString.Builder.Char.Utf8:
    fromText

  -- | Defined in Control.Monad:
  , foldM
  , liftM

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

  -- | Defined in Data.ByteString.Lazy:

  -- | Defined in Data.List:
  , nub

  -- | Defined in Data.List.Utils:
  , hasKeyAL
  , keysAL 

  -- | Defined in Data.Maybe:
  , fromJust
  , fromMaybe

  -- | Defined in Data.Text:
  , tConcat
  , tPack
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

  -- | Defined below:
--  , lookupViewDictionary
  , cookieToBS
  , decdoc
  , encdoc
  , lookupBsonVal

)  where

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Monad (foldM, liftM)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified 
       Data.Bson as Bson
import Data.Bson.Binary (putDocument, getDocument)
import qualified 
       Data.ByteString.Char8 as BS (concat,intercalate,empty,tail,span)
import Data.ByteString.Lazy (toChunks,fromChunks)
import Data.Dynamic (fromDyn, fromDynamic, toDyn)
import Data.List (nub)
import Data.List.Utils (hasKeyAL, keysAL)
import Data.Maybe (fromJust,fromMaybe)
import qualified 
       Data.Text as T (concat, pack, toLower, intercalate, lines, unlines,
       splitOn, replace, append, take, words, )
import qualified 
       Data.Text.IO as T (readFile,writeFile,putStrLn)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hell.Conf 
import Hell.Types
import Network.HTTP.Types.Header ( hCookie ) 
import Network.Wai.Handler.Warp (run)
import Web.ClientSession ()

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

tConcat :: [Text] -> Text
tConcat = T.concat

tToLower :: Text -> Text
tToLower = T.toLower

tPack :: String -> Text
tPack = T.pack

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
