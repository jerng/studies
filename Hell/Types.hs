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
  , ByteString

  -- | Defined in Data.Text:
  , Text
  
  -- | Defined in Data.Dynamic:
  , Dynamic
  , Typeable

  -- | Defined in Data.Map:
  , Map

  -- | Defined in Data.Word:
  , Word8

  -- | Defined in Network.Wai
  , Request (..)
  , Response (..)
  , Status (..)

  -- | Defined in Network.HTTP.Types.Header
  , Header

  -- | Defined in Network.HTTP.Types
  , accepted202
  , ok200

  -- | Defined below:
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
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Dynamic (Dynamic, Typeable)
import Data.Word (Word8)
import Network.Wai (Request(..), Response(..))
import Network.HTTP.Types 
  ( Status
  , accepted202
  , ok200
  )
import Network.HTTP.Types.Header ( Header) 

type BsonBinary = Binary

type ResourceNameText = Text 
type ControllerName = Text
type ActionName = Text
type Route = (ControllerName,ActionName)

type Action = Report -> Report

type CookieAttribute = ByteString
type CookieValue = ByteString
type CookieAVPair = (CookieAttribute, CookieValue)

data Cookie = Cookie  { cookieName :: CookieAttribute -- essential
                      , cookieValue :: CookieValue -- essential
                      , cookieSecure :: Bool
                      , cookieHttpOnly :: Bool
                      , cookiePairs :: [CookieAVPair]
                      }

type ReportM = [(Text,Report)]
{- I am giving serious thought to naming this data structure: (Hell).

Record syntax may be useful in this context, as the Report will be updated
at various points during the (Hell.Server) response.

-}

data Report = Report
  { session :: Document 
  , request :: Maybe Request
      -- Network.Wai.Request
  , actRoute :: Route -- of Action
      -- We should only ever need one. To redirect from one to another, use a
      -- status300!
  , viewRoute :: Route -- of View
      -- Again, we should only ever need one.  Addresses of subViews/widgets,
      -- in future development, should be communicated via the ViewDictionary.
  , actBson :: Document -- ActionDictionary
      -- This should be the medium of communicating most data from the 
      -- Server through to the Controller layer.
      -- Perhaps rename to (dataA).
  , viewBson :: Document -- ViewDictionary
      -- This should be the medium of communicating most data from the 
      -- Controller layer to the View layer.
      -- Perhaps rename to (dataV).
  , meta :: Text
      -- This should move into the session when we have that.
      --
      -- This should be a single message. (Feel free to debate the merits of a
      -- list of messages that gets processed into one, later.) The message
      -- should contain metadata (information) about the Request/Response, 
      -- which  will be shown to the User regardless of which View is ultimately
      -- rendered to the User. CakePHP calls this a Flash message. 
  , status :: Status
      -- Network.Wai.Status
  , resHeaders :: [Header]
      -- Network.HTTP.Headers.Header
  -- , session?
  -- , cookies?
  -- , other?
  , subReports :: ReportM
      -- Outstanding views, which need to be rendered, then inserted into the
      -- current View stipulated in viewRoute.
  , viewTemplate :: Maybe Route
      -- experiments with templates.
  }

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
                  deriving (Eq,Show) 

data SliceTag = ImportControllers 
              | ImportViews
              | ListActions
              | ListViews
              deriving (Eq,Show)

data Slice  = Slice ResourceName SliceTag
              deriving (Show)

data Unrendered = Plain Text 
                | Exp Text
                deriving (Show)

data AppMode  = Development
              | Production
              deriving (Eq,Show)

