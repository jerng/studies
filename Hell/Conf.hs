{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hell.Conf where

import Data.List
import qualified 
       Data.ByteString as BS
import qualified 
       Data.Text as T
import Hell.Types
import System.Directory

hellServerPort :: Int
hellServerPort = 3000

appMode :: AppMode
appMode = --Development0
          -- Development1
           Development2
          -- Production

-- I would really like to know how all this setting of defaults
-- affects memory use. Testing will be required.
defaultReport :: Report
defaultReport = Report  { request = Nothing
                        , session = []
                        , actBson = [] 
                        , actRoute = defaultRoute
                        , viewRoute = defaultRoute
                        , viewBson = []
                        , subReports = []
                        , meta = ""
                        , status = defaultStatus 
                        , resHeaders = defaultHeaders
                        , viewTemplate = defaultViewTemplate
                        , pathVars = []
                        , static = False
                        , debug = []
                        , action = id 
                        }

sessionCookieName :: ByteString
sessionCookieName = "FsbD"

defaultCookie :: Cookie
defaultCookie = Cookie  { cookieName = sessionCookieName
                        , cookieValue = "" 
                        , cookieHttpOnly = False
                        , cookieSecure = False
                        , cookiePairs = 
                          [ ("Max-Age","10080")
                          --, ("path","/")
                          --, ("Max-Age","0")
                          --, ("Domain","localhost")

                          ]
                        }

metaNoSuchAction :: Text
metaNoSuchAction = "We could not find the page that you are looking for." 
  `T.append` if appMode == Production then "" else 
    "<div class=\"debug\">Hell.Server.confirmAction did not find the\
    \ requested action in Hell.Server.actionList. This is the list assembled\
    \ by ./makeHell.hs and spliced into ./app/Server.hs.</div>"

keyOfMetaView :: Text
keyOfMetaView = "metaView"

-- | The key in a viewBson of a view template, whose value is the text 
-- of a view that has been full rendered, along with any of its subviews. 
-- (Report and sub Reports) by Hell.Server.renderReport.
keyOfTemplatedView :: Text
keyOfTemplatedView = "viewContent"

defaultViewTemplate :: Maybe Route
defaultViewTemplate = Just ("default","template")

defaultHeaders :: [Header]
defaultHeaders = []

defaultStatus :: Status
defaultStatus = accepted202

indexAction :: ActionName
indexAction = "index"

defaultRoute :: Route
defaultRoute = ("default","index")

missingActionRoute :: Route
missingActionRoute = ("default","nosuchaction")

-- | Scaffolding. This should just be passed as a message.
missingViewRoute :: Route
missingViewRoute = ("default","nosuchview")

staticFileRoute :: Route
staticFileRoute = ("default","files")

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
fromPath r = case r of
  Files          -> "./src/f/"
  Controllers    -> "./src/c/"
  Models         -> "./src/m/"
  Views          -> "./src/v/"
  Hell           -> "./Hell/"
  Conf           -> "./Hell/Conf.hs"
  Lib            -> "./Hell/Lib.hs"
  Splice         -> "./Hell/Splice.hs"
  Types          -> "./Hell/Types.hs"
  Server         -> "./Hell/template.Server.hs"
  AppController  -> "./src/AppController.hs"

toPath :: ResourceName -> FilePath
toPath r = case r of
  App             -> "./app/"
  Files           -> "./app/Files/"
  Controllers     -> "./app/Controllers/"
  Models          -> "./app/Models/"
  Views           -> "./app/Views/"
  Hell            -> "./app/Hell/"
  Conf            -> "./app/Hell/Conf.hs"
  Lib             -> "./app/Hell/Lib.hs"
  Splice          -> "./app/Hell/Splice.hs"
  Types           -> "./app/Hell/Types.hs"
  Server          -> "./app/Server.hs"
  AppController   -> "./app/AppController.hs"

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
takeUntilDot filePath = takeWhile ('.'/=) filePath

messageStartMakeHell :: Text
messageStartMakeHell = 
  "\nAssembling app. Reading from ./src and writing to ./app ..."

messageStartTryHell :: Text
messageStartTryHell = "Running source code of ./app/Server.hs ...\n"

messageJobDone :: Text
messageJobDone = "... and the job is done.\n"

viewBsonHelpers :: Text 
viewBsonHelpers = 
  "  view label = lookupBsonVal label (viewBson report)"
--  "   viewInt key = fromMaybe (error $ \"viewInt key missing in action, and view: \" ++ show (actRoute report) ++ show (viewRoute report)) $\
--  \     lookupBsonVal key (viewBson report) :: Int\n\
--  \   viewFloat key = fromMaybe (error $ \"viewFloat key missing in action, and view: \" ++ show (actRoute report) ++ show (viewRoute report))  $\
--  \     lookupBsonVal key (viewBson report) :: Float\n\
--  \   viewText key = fromMaybe (error $ \"viewText key missing in action, and view: \" ++ show (actRoute report) ++ show (viewRoute report))  $\
--  \     lookupBsonVal key (viewBson report) :: Text\n\
--  \   viewIntList key = fromMaybe (error $ \"viewIntList key missing in action, and view: \" ++ show (actRoute report) ++ show (viewRoute report))  $\
--  \     lookupBsonVal key (viewBson report) :: [Int]\n"

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
