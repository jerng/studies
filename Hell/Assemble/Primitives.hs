module Hell.Assemble.Primitives
  ( module Hell.Types
  , controllers
  , models
  , views
  , scriptExtension
  , viewExtension
  , fromPath
  , toPath
  , staticResources
  , templatedResources
  ) where

import Data.List (isSuffixOf,delete)
import Hell.Types
import System.Directory (getDirectoryContents)

-- | Notice that (Hell.Splice) isn't included here.
-- That's because it's not used in ./app .
staticResources :: [ResourceName]
staticResources = 
  [ Attributed
  , Conf
  , Debug
  , ParseForms
  , ParseHeaders
  , Lib
  , Show
  , Types
  , AppController
  , AppModel
  ]

templatedResources :: [ResourceName]
templatedResources = 
  [ Server
  ]

fromPath :: ResourceName -> FilePath
fromPath r = case r of
  Files           -> "./src/f/"
  Controllers     -> "./src/c/"
  Models          -> "./src/m/"
  Views           -> "./src/v/"
  Hell            -> "./Hell/"
  HellParse       -> "./Hell/Parse/"
  Attributed      -> "./Hell/Attributed.hs"
  Conf            -> "./Hell/Conf.hs"
  Debug           -> "./Hell/Debug.hs"
  ParseForms      -> "./Hell/Parse/Forms.hs"
  ParseHeaders    -> "./Hell/Parse/Headers.hs"
  Lib             -> "./Hell/Lib.hs"
  Show            -> "./Hell/Show.hs"
  Splice          -> "./Hell/Splice.hs"
  Types           -> "./Hell/Types.hs"
  Server          -> "./Hell/template.Server.hs"
  AppController   -> "./src/AppController.hs"
  AppModel        -> "./src/AppModel.hs"

toPath :: ResourceName -> FilePath
toPath r = case r of
  App             -> "./app/"
  Files           -> "./app/Files/"
  Controllers     -> "./app/Controllers/"
  Models          -> "./app/Models/"
  Views           -> "./app/Views/"
  Hell            -> "./app/Hell/"
  HellParse       -> "./app/Hell/Parse/"
  Attributed      -> "./app/Hell/Attributed.hs"
  Conf            -> "./app/Hell/Conf.hs"
  Debug           -> "./app/Hell/Debug.hs"
  ParseForms      -> "./app/Hell/Parse/Forms.hs"
  ParseHeaders    -> "./app/Hell/Parse/Headers.hs"
  Lib             -> "./app/Hell/Lib.hs"
  Show            -> "./app/Hell/Show.hs"
  Splice          -> "./app/Hell/Splice.hs"
  Types           -> "./app/Hell/Types.hs"
  Server          -> "./app/Server.hs"
  AppController   -> "./app/AppController.hs"
  AppModel        -> "./app/AppModel.hs"


controllers :: IO [FilePath]
controllers = do  
  contents <- getDirectoryContents $ fromPath Controllers 
  return $ map takeUntilDot $ filterInScripts contents

models :: IO [FilePath]
models = do  
  contents <- getDirectoryContents $ fromPath Models 
  return $ map takeUntilDot $ filterInScripts contents

views :: IO [(FilePath, [FilePath])]
views = (mapM eachCdir) =<< cDirs
  where
    cDirs = do 
      contents <- getDirectoryContents $ fromPath Views
      return $ delete ".." $ delete "." contents
    eachCdir cDir = do
      contents' <- getDirectoryContents $ fromPath Views ++ cDir
      return  
        ( cDir
        , map takeUntilDot $ filterInViews contents' 
        )

scriptExtension :: FilePath
scriptExtension = ".hs"

viewExtension :: FilePath
viewExtension = ".view"

takeUntilDot :: FilePath -> FilePath
takeUntilDot filePath = takeWhile ('.'/=) filePath

filterInScripts :: [FilePath] -> [FilePath]
filterInScripts filePathList = filter (isSuffixOf scriptExtension) filePathList

filterInViews :: [FilePath] -> [FilePath]
filterInViews filePathList = filter (isSuffixOf viewExtension) filePathList

