{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Hell.Assemble

main :: IO ()
main = do 
  putStrLn "\nAssembling app. Reading from ./src and writing to ./app ..."
  do  resetAppDir
      copyStaticFiles -- combine with above?
      copyStaticResources
      assembleTemplatedResources
      assembleControllers
      assembleViews
      assembleModels
  putStrLn "... and the job is done.\n"
