{-# LANGUAGE OverloadedStrings #-} 

module MakeHell where

import Data.List
import Data.String.Utils
import qualified Data.Text    as T 
import qualified Data.Text.IO as T
import Hell.Lib
import Hell.Splice (spliceController,spliceTemplate,spliceView)
import System.Directory

-------------------------------------------------------------------------------
main :: IO ()
main = do 
  T.putStrLn messageStartMakeHell
  resetAppDir
  copyStaticResources
  assembleControllers
  assembleViews
  assembleModels
  assembleTemplates
  T.putStrLn messageJobDone 

resetAppDir :: IO ()
resetAppDir = do 
  bool <- doesDirectoryExist (toPath App)
  case bool of  
    False ->  return ()
    True  ->  do
      removeDirectoryRecursive (toPath App)

--      bool' <- doesDirectoryExist (toPath App)
          -- When the directory existed and was deleted, bool' is True.
          -- I'm not sure if the result of (doesDirectoryExist) is 
          -- cached and reused, or if the filesystem has not yet
          -- registered the deletion, before this check occurs.
--      case bool of  
--        False ->  return ()
--        True  ->  error "\nERROR: (resetAppDir) \
--          \ could not remove the existing App directory."
  
  mapM_ (createDirectoryIfMissing True) 
        [ toPath Hell
        , toPath Controllers
        , toPath Views
        , toPath Models
        ]

copyStaticResources :: IO ()
copyStaticResources = mapM_ copy staticResources
  where copy = \module'-> copyFile 
                          (fromPath module')
                          (toPath module')

assembleControllers :: IO ()
assembleControllers = do
  cs <- controllers
  let assemble c =  do
        splicedText <- spliceController c
        T.writeFile (toPath Controllers ++ c ++ scriptExtension) $
          T.concat  [ "{-# LANGUAGE OverloadedStrings #-}\n\n\
                      \module Controllers.", T.pack c, " where\n\n\
                      \import qualified Data.Text as T\n\
                      \import Hell.Lib\n\n", splicedText ]
  mapM_ assemble cs

assembleModels :: IO ()
assembleModels = (mapM_ copy) =<< models
  where copy = \m ->  copyFile  
                      (fromPath Models ++ m ++ scriptExtension)
                      (toPath Models ++ m ++ scriptExtension)  

-- TODO: WARN for all actions without views.
--       WARN for all views without actions.
assembleViews :: IO ()
assembleViews = do 
  al <- views
  let cs = keysAL al
      eachC = \c -> do 
        createDirectoryIfMissing False $ toPath Views ++ c
        let vs = fromJust $ lookup c al 
        mapM_ (eachV c) vs
      eachV = \c v -> do
        unsplicedText <- T.readFile $ concat 
          [fromPath Views, c, "/", v, scriptExtension, viewExtension]
        let splicedText = spliceView c v unsplicedText
            toFileName = v ++ scriptExtension
            toFilePath = concat [ toPath Views, c, "/", toFileName ]
        T.writeFile toFilePath splicedText 
  mapM_ eachC cs
  
assembleTemplates :: IO ()
assembleTemplates = mapM_ assemble templatedResources 
  where assemble =  \module' -> do
          splicedText <- spliceTemplate module'
          T.writeFile (toPath module') splicedText
