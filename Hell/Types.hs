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

  -- | Defined in Network.HTTP.Types
  , ok200

  -- | Defined below:
  , DM
  , ControllerName
  , ActionName
  , Route
  , AppControllerVars
  , ViewDictionary
  , Report (..)
  , ResourceName (..)
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
  , ok200
  )

type ResourceNameText = Text 
type ControllerName = Text
type ActionName = Text
type Route = (ControllerName,ActionName)

-- | Replace DM with Data.Map.Map
type DM = [(Text,Dynamic)]
type AppControllerVars = DM
type ViewDictionary = DM 

data Report = Report Status Route ViewDictionary

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
