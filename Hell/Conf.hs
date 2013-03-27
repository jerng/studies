{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hell.Conf where

import Data.List
import qualified Data.Text as T
import Hell.Types
import System.Directory

hellServerPort :: Int
hellServerPort = 3000

-- I would really like to know how all this setting of defaults
-- affects memory use. Testing will be required.
defaultReport = Report 
  { request = Nothing
  , actionDictionary = [] 
  , routeA = defaultRoute
  , routeV = defaultRoute
  , viewDictionary = []
  , subReports = []
  , meta = ""
  , status = defaultStatus 
  , headers = defaultHeaders
  }


defaultHeaders :: [Header]
defaultHeaders = []

defaultStatus :: Status
defaultStatus = accepted202

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
  , AppController
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
fromPath Server         = "./Hell/template.Server.hs"
fromPath AppController  = "./src/AppController.hs"

toPath :: ResourceName -> FilePath
toPath App              = "./app/"
toPath Controllers      = "./app/Controllers/"
toPath Models           = "./app/Models/"
toPath Views            = "./app/Views/"
toPath Hell             = "./app/Hell/"
toPath Conf             = "./app/Hell/Conf.hs"
toPath Lib              = "./app/Hell/Lib.hs"
toPath Splice           = "./app/Hell/Splice.hs"
toPath Types            = "./app/Hell/Types.hs"
toPath Server           = "./app/Server.hs"
toPath AppController    = "./app/AppController.hs"

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
  "  viewInt key = lookupViewDictionary key (viewDictionary report) :: Int\n\
  \  viewFloat key = lookupViewDictionary key (viewDictionary report) :: Float\n\
  \  viewText key = lookupViewDictionary key (viewDictionary report) :: Text\n\
  \  viewIntList key = lookupViewDictionary key (viewDictionary report) :: [Int]\n"
  -- TODO: Try and see if a class can introduce ad hoc polymorphism here.

class ViewExpression a where
  toText :: a -> Text

instance ViewExpression Int where
  toText a = T.pack $ show a

instance ViewExpression Float where
  toText a = T.pack $ show a

instance ViewExpression Text where
  toText a = a

instance ViewExpression [Int] where
  toText a = T.pack $ show a
