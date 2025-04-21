import System.Process
import System.Environment
import Text.Regex
import Data.Maybe
import Control.Monad.IO.Class

--main :: IO ()
--main = getPackage package
--package :: IO String
--package = do  args <- getArgs
--              return $ head args 

main :: IO [()]
-- | Add something to call "cabal update" first
main = mapM getPackage packages
packages :: [IO String]
packages = map return [
  "hspec-1.4.2"
  ,"http-conduit-1.8.4.3"
  ,"yesod-platform"
  ]

getPackage :: IO String -> IO ()
getPackage pIO = do 
  pPure <- pIO
  putStrLn $ "Getting: \"" ++ pPure ++ "\"..."
  (_,_,result) <- readProcessWithExitCode
    
    -- | Installing to the global DB may confuse GHC; install to local also
    -- | ~/.cabal/config will also enable presets instead of flags
    --"sudo" -- global 
    "cabal" -- local
    [ --"cabal", --global
      "install", --local
      --"--global", -- global
      "--reinstall", 
      "--force-reinstalls", 
      --"--enable-shared", 
      pPure
      ] 
    ""

  -- | add this pattern to search for: 
  -- |  /usr/bin/ld: cannot find -lHStransformers-base-0.4.1-ghc7.4.2


  let regex = mkRegex "Perhaps you haven't installed the \\\"dyn\\\"\
    \ libraries for package `(.*?)'\\?"

  let dpPure = head $ fromMaybe [""] $  matchRegex regex result

  case length dpPure of

    0 ->  putStrLn $ "\"" ++ pPure ++ 
            "\" \"dyn\" library processed; may not have succeeded,\
            \but did not yell any dependency errors.\n"

    _ ->  do  putStrLn $ "\"" ++ pPure ++ "\" depends on : \"" ++ dpPure ++ 
                "\"\n"
              getPackage $ return dpPure
              getPackage pIO 

