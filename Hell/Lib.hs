{-# LANGUAGE OverloadedStrings #-}

module Hell.Lib (
  
  -- | Defined in Blaze.ByteString.Builder.Char.Utf8:
    fromText

  -- | Control.Monad.Trans.Resource:
  , ResourceT

  -- | Defined in Data.Dynamic:
  , toDyn

  -- | Defined in Data.Dynamic:
  , fromJust

  -- | Defined in Data.List.Utils:
  --, replace

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

  -- | Defined in Hell.Splice:
  , spliceTemplate
  , spliceView

  -- | Defined in Hell.Types:
  , Text
  , Request (..)
  , Response (..)
  , Status (..)

  , DM
  , Controller
  , Action
  , Route
  , AppControllerVars
  , ViewSlices
  , Reaction (..)
  , ResourceName (..)
  , Slice (..)
  , SliceID (..)
  , Unrendered (..)
  , ResourceNameText
  
  -- | Defined in Network.HTTP.Types:
  , run

  -- | Defined below:
  , lookupDM
  , lookupDM_
)  where

-- import qualified Data.Typeable
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Dynamic (fromDynamic, toDyn)
--import Data.List.Utils (replace)
import Data.Maybe (fromJust)
import Hell.Conf 
import Hell.Splice (spliceTemplate, spliceView)
import Hell.Types
import Network.Wai.Handler.Warp

lookupDM :: (Typeable a) => Text -> [(Text, Dynamic)] -> a
lookupDM k dmap    = Data.Maybe.fromJust 
                    $ Data.Dynamic.fromDynamic 
                    $ Data.Maybe.fromJust 
                    $ lookup k dmap
{-

    lookupDM_ ::

    lookupDM_ can be used for more keys than just Text keys. I'm not sure if I
    want to expand DMs into a class to include numeric and other keys, or if I
    want to reduce the functionality of lookupDM, or if this current state is
    just peachy.

-}
lookupDM_ :: (Eq a1, Typeable a) => a1 -> [(a1, Dynamic)] -> a
lookupDM_ k dmap    = Data.Maybe.fromJust 
                    $ Data.Dynamic.fromDynamic 
                    $ Data.Maybe.fromJust 
                    $ lookup k dmap

