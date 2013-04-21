module Hell.Types (
  
  -- | Defined in Control.Monad.Trans.Resource:
    ResourceT

  -- | Defined in Data.Binary:
  , Bin.Binary 

  -- | Defined in Data.Bson:
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

  -- | Defined in Data.ByteString:
  , BS.ByteString

  -- | Defined in Data.ByteString:
  , LByteString

  -- | Defined in Data.Text:
  , Text
  
  -- | Defined in Data.Map:
  , Map

  -- | Defined in Data.Vault:
  , Vault (..)

  -- | Defined in Data.Word:
  , Word8

  -- | Defined in Network.Wai
  , RequestBodyLength (..)
  , Request (..)
  , Response (..)
  , Status (..)

  -- | Defined in Network.Wai.Handler.Warp
  , Settings

  -- | Defined in Network.HTTP.Types.Header
  , Header
--  , RequestHeaders

  -- | Defined in Network.HTTP.Types
  , QueryItem
  , accepted202
  , ok200
  , found302

  -- | Defined in Network.Wai.Parse
  , File
  , FileInfo (..)
  , Param
  , BackEnd

  -- | Defined in Web.ClientSession
  , IV
  , Key

  -- | Defined below:
  , ReportHandler
  , Action
  , ResourceName (..)
  , ControllerName
  , ActionName
  , Route

  , ReportM

  , Report (..)
  , SliceTag (..)
  , Slice (..)
  , Unrendered (..)
  , ResourceNameText
  , AppMode (..)

  , CookieAttribute
  , CookieValue
  , CookieAVPair
  , Cookie (..)

) where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Binary as Bin (Binary) 
import Data.Bson as Bson 
import qualified Data.ByteString.Char8 as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vault as V (Vault(..))
import Data.Word (Word8)
import Network.HTTP.Types 
  ( Status
  , Query
  , QueryItem
  , accepted202
  , ok200
  , found302
  )
import Network.HTTP.Types.Header ( Header{-,RequestHeaders-}) 
import Network.Wai (RequestBodyLength(..),Request(..),Response(..))
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Parse (Param, File, FileInfo(..), BackEnd)
import Web.ClientSession (IV,Key)

type LByteString = LBS.ByteString
type BsonBinary = Binary
type ResourceNameText = Text 
type ControllerName = Text
type ActionName = Text
type Route = (ControllerName,ActionName)
type ReportM = [(Text,Report)]
type ReportHandler = Report -> Report
type Action = ReportHandler 
type CookieAttribute = BS.ByteString
type CookieValue = BS.ByteString
type CookieAVPair = (CookieAttribute, CookieValue)

-- Hell.Splice.Types
data SliceTag = ImportControllers 
              | ImportViews
              | ListActions
              | ListViews
              deriving (Eq,Show)

-- Hell.Splice.Types
data Slice  = Slice ResourceName SliceTag
              deriving (Show)

-- Hell.Splice.Types
data Unrendered = Plain Text 
                | Exp Text
                deriving (Show)

data ResourceName = Controllers
                  | Models
                  | Views
                  | App
                  | Hell
                  | Server 
                  | Conf
                  | Lib
                  | Splice
                  | Types
                  | AppController
                  | AppModel
                  | Files
                  deriving (Eq,Show) 

data AppMode  = FullAutoDebug
              | SemiAutoDebug
              | ManualDebug 
              | Production
              deriving (Eq,Ord,Show)

data Cookie = Cookie  { cookieName :: CookieAttribute -- essential
                      , cookieValue :: CookieValue -- essential
                      , cookieSecure :: Bool
                      , cookieHttpOnly :: Bool
                      , cookiePairs :: [CookieAVPair]
                      }

data Report = Report
  { 
    -- REQUEST ****************************************************************
    request :: Maybe Request
  , shownRequest :: String
  , pathVars :: [Text]
      -- (pathInfo someRequest) less the first two heads (con:act:pathVars)
  , reqCookies :: [(CookieAttribute,CookieValue)]
  , session :: Document 
  , params :: [Param]
  , files :: [File LBS.ByteString]

    -- BUSINESS LOGIC ********************************************************
  , static :: Bool
      -- for serving static files
  , form_ :: Document
      -- a construct, that depends on the request method; TODO: validate this
  , data_ :: Document
      -- Goto for form data, model data, like CakePHP's $controller->data 
  , actRoute :: Route -- of Action
      -- We should only ever need one. To redirect from one to another, use a
      -- status302!
  , action :: Action
  , subReports :: ReportM
      -- Outstanding views, which need to be rendered, then inserted into the
      -- current View stipulated in viewRoute.

    -- RESPONSE **************************************************************
  , viewRoute :: Route -- of View
      -- Again, we should only ever need one.  Addresses of subViews/widgets,
      -- in future development, should be communicated via the ViewDictionary.
  , viewTemplateRoute :: Maybe Route
      -- experiments with templates.
  , status :: Status
      -- Network.Wai.Status
  , resCookies :: [Cookie]
      -- At some point these get added to resHeaders
  , resHeaders :: [Header]
      -- Network.HTTP.Headers.Header
  , viewBson :: Document -- ViewDictionary
      -- This should be the medium of communicating most data from the 
      -- Controller layer to the View layer.
      -- Perhaps rename to (dataV).
  , debug :: [Text]
      -- bit'o'a dumpy arrangement for now

    -- UTILITY HEAP **********************************************************
  , key :: Maybe Key
  , iv :: Maybe IV
  } 
  --deriving (Show)

