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
import Network.HTTP.Types ( 
    Status
  , ok200
  )

type ResourceNameText = Text 
type DM = [(Text,Dynamic)]
type ControllerName = Text
type ActorName = Text
type Route = (ControllerName,ActorName)
type AppControllerVars = DM
type ViewDictionary = [(Text,Dynamic)]

data Action = Action Status Route ViewDictionary

-- | This data structure was considered. 
-- But I can't think of a great use for it.
--data Resource = Resource  { name :: ResourceName
--                          , src :: FilePath
--                          , app :: FilePath 
--                          } 

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
