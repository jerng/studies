module Hell.Types (
  
  -- | Defined in Data.Text:
    Text
  
  -- | Defined in Data.Dynamic:
  , Dynamic
  , Typeable

  -- | Defined in Network.Wai
  , Request (..)
  , Response (..)
  , Status (..)

  -- | Defined in Network.HTTP.Headers
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

  , DM
  , ActionDictionary
  , ViewDictionary
  , ReportM

  , Report (..)
  , SliceTag (..)
  , Slice (..)
  , Unrendered (..)
  , ResourceNameText

) where

import Data.Text (Text)
import Data.Dynamic (Dynamic, Typeable)
import Network.Wai (Request(..), Response(..))
import Network.HTTP.Types 
  ( Status
  , accepted202
  , ok200
  )
import Network.HTTP.Headers (Header) 

type ResourceNameText = Text 
type ControllerName = Text
type ActionName = Text
type Route = (ControllerName,ActionName)

type Action = Report -> Report

-- | Replace DM with Data.Map.Map
type DM = [(Text,Dynamic)]
type ActionDictionary = DM
type ViewDictionary = DM 
type ReportM = [(Text,Report)]
{- I am giving serious thought to naming this data structure: (Hell).

Record syntax may be useful in this context, as the Report will be updated
at various points during the (Hell.Server) response.

-}

data Report = Report
  { request :: Maybe Request,
      -- Network.Wai.Request
    routeA :: Route -- of Action
      -- We should only ever need one. To redirect from one to another, use a
      -- status300!
  , routeV :: Route -- of View
      -- Again, we should only ever need one.  Addresses of subViews/widgets,
      -- in future development, should be communicated via the ViewDictionary.
  , actionDictionary :: ActionDictionary
      -- This should be the medium of communicating most data from the 
      -- Server through to the Controller layer.
      -- Perhaps rename to (dataA).
  , viewDictionary :: ViewDictionary
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
  , headers :: [Header]
      -- Network.HTTP.Headers.Header
  -- , session?
  -- , cookies?
  -- , other?
  , subReports :: ReportM
      -- Outstanding views, which need to be rendered, then inserted into the
      -- current View stipulated in routeV.
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
