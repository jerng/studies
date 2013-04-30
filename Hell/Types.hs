-- {-# LANGUAGE ExistentialQuantification #-}

module Hell.Types 
  ( CookieAttribute
  , CookieValue
  , CookieAVPair
  , Cookie (..)
  , Report (..)
  , ReportHandler
  , Action
  , ResourceName (..)
  , ControllerName
  , ActionName
  , Route
  , ResourceNameText
  , AppMode (..)
  ) where

import Data.Bson as Bson (Document)
import qualified Data.ByteString.Char8 as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString)
import Data.Text (Text)
import Network.HTTP.Types ( Status )
import Network.HTTP.Types.Header ( Header ) 
import Network.Wai (Request(..))
import Network.Wai.Parse (Param, File )
import Web.ClientSession (IV,Key)

type ResourceNameText = Text 
type ControllerName = Text
type ActionName = Text
type Route = (ControllerName,ActionName)

data ResourceName = Controllers
                  | Models
                  | Views
                  | App
                  | Hell
                  | HellParse
                  | Attributed
                  | Conf
                  | Debug
                  | ParseForms
                  | ParseHeaders
                  | Lib
                  | Show
                  | Server 
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
type CookieAttribute = BS.ByteString
type CookieValue = BS.ByteString
type CookieAVPair = (CookieAttribute, CookieValue)

type Action = ReportHandler 
type ReportHandler = Report -> Report
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
  , subReports :: [(Text,Report)] 
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
  , viewData_ :: Document -- ViewDictionary
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
