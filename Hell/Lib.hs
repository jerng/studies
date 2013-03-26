{-# LANGUAGE OverloadedStrings #-}

module Hell.Lib (
  
  -- | Defined in Blaze.ByteString.Builder.Char.Utf8:
    fromText

  -- | Defined in Control.Monad:
  , foldM
  , liftM

  -- | Defined in Control.Monad.Trans.Resource:
  , ResourceT

  -- | Defined in Data.Dynamic:
  , toDyn

  -- | Defined in Data.Maybe:
  , fromJust
  , fromMaybe

  -- | Defined in Data.List:
  , nub

  -- | Defined in Data.List.Utils:
  , keysAL 

  -- | Defined in Hell.Conf:
  , hellServerPort
  , defaultHeaders
  , defaultStatus
  , indexAction
  , defaultRoute
  , noSuchActionRoute
  , noSuchViewRoute

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
  , scriptExtension
  , viewExtension
  , viewDictionaryHelpers
  , ViewExpression

  -- | Defined in Hell.Types:
  , Text
  , Request (..)
  , Response (..)
  , Status (..)
  , Header
  , accepted202
  , ok200

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
  
  -- | Defined in Network.HTTP.Types:
  , run

  -- | Defined below:
  , lookupViewDictionary
  , toText

)  where

-- import qualified Data.Typeable
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Monad (foldM, liftM)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Dynamic (fromDynamic, toDyn)
import Data.List (nub)
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
