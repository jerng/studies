{-# LANGUAGE OverloadedStrings #-}

module Hell.Lib (
  
  -- | Defined in Blaze.ByteString.Builder.Char.Utf8:
    fromText

  -- | Defined in Control.Monad:
  , foldM
  , liftM

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
  
  -- | Defined in Network.HTTP.Types:
  , run

  -- | Defined in Web.ClientSession 

  -- | Defined below:
  , lookupViewDictionary
  , cookieToBS

)  where

-- import qualified Data.Typeable
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Monad (foldM, liftM)
import Data.Dynamic (fromDynamic, toDyn)
import Data.List (nub)
import Data.List.Utils (hasKeyAL, keysAL)
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Hell.Conf 
import Hell.Types
import Network.Wai.Handler.Warp
import Web.ClientSession

lookupViewDictionary :: (Typeable a) => Text -> ViewDictionary -> a
lookupViewDictionary k vd = 
  fromJust $ 
  fromDynamic $ 
  fromMaybe (toDyn ("(lookupViewDictionary returned Nothing)" :: Text)) $ 
  lookup k vd

-- | Perhaps the entire Cooke type should be refactored with (Maybe)
cookieToBS :: Cookie -> ByteString
cookieToBS c = B.intercalate ";" $ concat
  [ [ B.append "name=" $ cookieName c ]
  , if cookieHttpOnly c then ["HttpOnly"] else []
  , if cookieSecure c then ["Secure"] else []
  , -- ["libtest=libval"] 
    map cookieAVPairToBS $ cookiePairs c
  ]

cookieAVPairToBS :: CookieAVPair -> ByteString
cookieAVPairToBS (attr,val) = B.concat [attr,"=",val] 
