{-# LANGUAGE OverloadedStrings #-}

module Hell.Conf where

import Data.List
import Hell.Types
import System.Directory

indexAction :: ActionName
indexAction = "index"

defaultRoute :: Route
defaultRoute = ("default","index")

noSuchActionRoute :: Route
noSuchActionRoute = ("default","nosuchaction")

-- | Scaffolding. This should just be passed as a message.
noSuchViewRoute :: Route
noSuchViewRoute = ("default","nosuchview")

-- | Notice that (Hell.Splice) isn't included here.
-- That's because it's not used in ./app .
staticResources :: [ResourceName]
staticResources = 
  [ Conf
  , Lib
  , Types
  ]

templatedResources :: [ResourceName]
templatedResources = 
  [ Server
  ]

sliceIDsOf :: ResourceName -> [Slice]
sliceIDsOf Server = 
  [ Slice Server ImportControllers 
  , Slice Server ImportViews 
  , Slice Server ListActions 
  , Slice Server ListViews
  ]

fromPath :: ResourceName -> FilePath
fromPath Controllers    = "./src/c/"
fromPath Models         = "./src/m/"
fromPath Views          = "./src/v/"
fromPath Hell           = "./Hell/"
fromPath Conf           = "./Hell/Conf.hs"
fromPath Lib            = "./Hell/Lib.hs"
fromPath Splice         = "./Hell/Splice.hs"
fromPath Types          = "./Hell/Types.hs"

toPath :: ResourceName -> FilePath
toPath App            = "./app/"
toPath Controllers    = "./app/Controllers/"
toPath Models         = "./app/Models/"
toPath Views          = "./app/Views/"
toPath Server         = "./app/Server.hs"
toPath Hell           = "./app/Hell/"
toPath Conf           = "./app/Hell/Conf.hs"
toPath Lib            = "./app/Hell/Lib.hs"
toPath Splice         = "./app/Hell/Splice.hs"
toPath Types          = "./app/Hell/Types.hs"

templateFromPath :: ResourceName -> FilePath
templateFromPath module' = 
  concat [fromPath Hell, "template.", show module', scriptExtension]

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

filterInScripts :: [FilePath] -> [FilePath]
filterInScripts filePathList = filter (isSuffixOf scriptExtension) filePathList

filterInViews :: [FilePath] -> [FilePath]
filterInViews filePathList = filter (isSuffixOf viewExtension) filePathList

takeUntilDot :: FilePath -> FilePath
takeUntilDot filePath = takeWhile ( \char -> not $ '.' == char ) filePath

messageStartMakeHell :: Text
messageStartMakeHell = 
  "\nAssembling app. Reading from ./src and writing to ./app ..."

messageStartTryHell :: Text
messageStartTryHell = "Running source code of ./app/Server.hs ...\n"

messageJobDone :: Text
messageJobDone = "... and the job is done.\n"

viewDictionaryHelpers :: Text 
viewDictionaryHelpers = 
  "  viewInt key = lookupViewDictionary key textMap :: Int\n\
  \  viewFloat key = lookupViewDictionary key textMap :: Float\n\
  \  viewText key = lookupViewDictionary key textMap :: Text\n\
  \  viewIntList key = lookupViewDictionary key textMap :: [Int]\n"

hellServerPort :: Int
hellServerPort = 3000
