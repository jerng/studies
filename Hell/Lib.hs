{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hell.Lib (
  
  -- | Defined in Blaze.ByteString.Builder.Char.Utf8:
    fromText

  -- | Defined in Control.Monad:
  , foldM
  , liftM

  -- | Defined in Data.Bson:
  , Document, (!?), look, {-lookup,-} valueAt, at, include, exclude, merge,
  Field(..), (=:), (=?), Label, Value(..), Val(..), fval, cast, typed,
  typeOfVal, {-Binary(..),-} Function(..), UUID(..), MD5(..), UserDefined(..),
  Regex(..), Javascript(..), Symbol(..), MongoStamp(..), MinMaxKey(..),
  ObjectId(..), timestamp, genObjectId

  -- | Defined in Data.ByteString:

  -- | Defined in Data.ByteString.Lazy:
  --, toChunks

  -- | Defined in Data.Dynamic:
  , toDyn

  -- | Defined in Data.Maybe:
  , fromJust
  , fromMaybe

  -- | Defined in Data.List:
  , nub

  -- | Defined in Data.List.Utils:
  , hasKeyAL
  , keysAL 

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
  , viewDictionaryHelpers
  , ViewExpression

  , toText

  -- | Defined in Hell.Types:
  , ResourceT
  , ByteString
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

  , DM
  , ActionDictionary
  , ViewDictionary
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
  
  -- | Defined in Network.Wai.Handler.Warp
  , run

  -- | Defined in Web.ClientSession 

  -- | Defined below:
  , lookupViewDictionary
  , cookieToBS
  , decdoc
  , encdoc

)  where

-- import qualified Data.Typeable
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Monad (foldM, liftM)
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)
import Data.Bson (Document)
import Data.Bson.Binary (putDocument, getDocument)
import qualified Data.ByteString as BS (concat,intercalate)
import Data.ByteString.Lazy (toChunks,fromChunks)
import Data.Dynamic (fromDyn, fromDynamic, toDyn)
import Data.List (nub)
import Data.List.Utils (hasKeyAL, keysAL)
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hell.Conf 
import Hell.Types
import Network.Wai.Handler.Warp (run)
import Web.ClientSession ()

lookupViewDictionary :: (Typeable a) => Text -> ViewDictionary -> a
lookupViewDictionary k vd = 
  fromJust $ 
  fromDynamic $ 
  fromMaybe (toDyn ("(lookupViewDictionary returned Nothing)" :: Text)) $ 
  lookup k vd

-- | Perhaps the entire Cooke type should be refactored with (Maybe)
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
