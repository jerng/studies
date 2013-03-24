{-# LANGUAGE OverloadedStrings #-}

module Hell.Lib (
  
  -- | Defined in Blaze.ByteString.Builder.Char.Utf8:
    fromText

  -- | Defined in Control.Monad:
  , foldM

  -- | Defined in Control.Monad.Trans.Resource:
  , ResourceT

  -- | Defined in Data.Dynamic:
  , toDyn

  -- | Defined in Data.Maybe:
  , fromJust

  -- | Defined in Data.List.Utils:
  , keysAL 

  -- | Defined in Hell.Conf:
  , defaultRoute
  , templatedResources
  , staticResources
  , sliceIDsOf
  , fromPath
  , toPath
  , templateFromPath
  , controllers
  , models
  , views
  , messageStartMakeHell
  , messageStartTryHell
  , messageJobDone
  , filterInViews
  , scriptExtension
  , viewExtension
  , viewDictionaryHelpers
  , hellServerPort

  -- | Defined in Hell.Types:
  , Text
  , Request (..)
  , Response (..)
  , Status (..)
  , ok200

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
  
  -- | Defined in Network.HTTP.Types:
  , run

  -- | Defined below:
  , lookupViewDictionary
)  where

-- import qualified Data.Typeable
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Monad (foldM)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Dynamic (fromDynamic, toDyn)
import Data.List.Utils (keysAL)
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.Text as T
import Hell.Conf 
import Hell.Types
import Network.Wai.Handler.Warp

lookupViewDictionary :: (Typeable a) => Text -> ViewDictionary -> a
lookupViewDictionary k vd = 
  fromJust $ 
  fromDynamic $ 
  fromMaybe (toDyn ("(lookupViewDictionary returned Nothing)" :: Text)) $ 
  lookup k vd
