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
  , ActorName
  , Route
  , AppControllerVars
  , ViewDictionary
  , Action (..)
  , ResourceName (..)
  , Slice (..)
  , SliceID (..)
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
type ActorName = Text
type Route = (ControllerName,ActorName)

-- | Replace DM with Data.Map.Map
type DM = [(Text,Dynamic)]
type AppControllerVars = DM
type ViewDictionary = DM 

data Action = Action Status Route ViewDictionary

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

data Slice  = ImportControllers 
            | ImportViews
            deriving (Eq,Show)

data SliceID  = SliceID ResourceName Slice
              deriving (Show)

data Unrendered = Plain Text 
                | Exp Text
                deriving (Show)
