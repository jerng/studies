{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Hell.Lib

-- | Other imports which would be dangerous to put into Hell.Lib;
-- but later, a lot of rethinking the name spaces for Hell in general
-- will be required.
import Data.List (partition)
import Hell.Splice (spliceController,spliceTemplate,spliceView)
import System.Directory 
  ( copyFile
  , removeDirectoryRecursive
  , createDirectoryIfMissing
  , doesFileExist
  , doesDirectoryExist
  )
import System.Path (recurseDir)
import System.IO.HVFS (SystemFS(..))

-------------------------------------------------------------------------------
main :: IO ()
main = do 
  tPutStrLn messageStartMakeHell
  resetAppDir
  copyStaticFiles -- combine with above?
  copyStaticResources
  assembleTemplatedResources
  assembleControllers
  assembleViews
  assembleModels
  tPutStrLn messageJobDone 

resetAppDir :: IO ()
resetAppDir = do 
  let dir = [ toPath Hell
            , toPath Controllers
            , toPath Views
            , toPath Models
            , toPath Files
            ]
      delete dir = do
--        putStrLn $ "resetting directory: " ++ dir
        bool <- doesDirectoryExist dir
        case bool of  
          False ->  return ()
          True  ->  do
            removeDirectoryRecursive dir
  mapM_ delete dir
  mapM_ (createDirectoryIfMissing True) dir
    -- combining deletion and creation of directories in the same map
      -- appears to result in non-sequential execution

copyStaticResources :: IO ()
copyStaticResources = mapM_ copy staticResources
  where 
    copy = \module'-> do
--      putStrLn $ "copying static resource: " ++ (show module')
      copyFile (fromPath module') (toPath module')

-- | Unoptimised, but frankly this file has a low priority for optimisation.
copyStaticFiles :: IO ()
copyStaticFiles = do
  paths <- recurseDir SystemFS $ fromPath Files
  let paths' = map (replace "//" "/") paths
  (dirs,files) <- partitionM doesDirectoryExist paths'
  flip mapM_ dirs 
    (\p-> createDirectoryIfMissing True $ 
            replace (fromPath Files) (toPath Files) p)
  flip mapM_ files 
    (\p-> do  
--      putStrLn $ "copying static file: " ++ (show p)
      copyFile p $ replace (fromPath Files) (toPath Files) p)

assembleControllers :: IO ()
assembleControllers = do
  cs <- controllers
  let assemble c =  do
--        putStrLn $ "assembling controller: " ++ c
        splicedText <- spliceController c
        tWriteFile (toPath Controllers ++ c ++ scriptExtension) $
          tConcat  [ "{-# LANGUAGE OverloadedStrings #-}\n\n\
                      \module Controllers.", tPack c, " where\n\n\
                      \import Hell.Lib\n\n", splicedText ]
  mapM_ assemble cs

assembleModels :: IO ()
assembleModels = (mapM_ copy) =<< models
  where copy = \m -> do
--          putStrLn $ "assembling model: " ++ m
          copyFile  (fromPath Models ++ m ++ scriptExtension)
                    (toPath Models ++ m ++ scriptExtension)  

-- TODO: WARN for all actions without views.
--       WARN for all views without actions.
assembleViews :: IO ()
assembleViews = do 
  al <- views
  let cs = keysAL al
      eachC = \c -> do 
        createDirectoryIfMissing False $ toPath Views ++ c
        let vs = fromMaybe 
              (error "confirmAct: Hell.Lib.missingActionRoute is\
                        \ itself missing. Shucks.") $ lookup c al 
        mapM_ (eachV c) vs
      eachV = \c v -> do
--        putStrLn $ "assembling view: " ++ c ++ "." ++ v
        unsplicedText <- tReadFile $ concat 
          [fromPath Views, c, "/", v, scriptExtension, viewExtension]
        let splicedText = spliceView c v unsplicedText
            toFileName = v ++ scriptExtension
            toFilePath = concat [ toPath Views, c, "/", toFileName ]
        tWriteFile toFilePath splicedText 
  mapM_ eachC cs
  
assembleTemplatedResources :: IO ()
assembleTemplatedResources = mapM_ assemble templatedResources 
  where assemble =  \module' -> do
--          putStrLn $ "assembling templated resource: " ++ (show module')
          splicedText <- spliceTemplate module'
          tWriteFile (toPath module') splicedText
