-- |  NOT A MODULE - just a bootstrap script, that runs ./app/ 

{-# LANGUAGE OverloadedStrings #-} 

import Control.Concurrent
import Control.DeepSeq (rnf)
import Control.Exception 
  (SomeException, mask, try, onException, throwIO, evaluate)
import Control.Monad
import Control.Monad.Error
import qualified Data.Text.IO as T
import Hell.Lib
import GHC.IO.Exception ( ioException, IOErrorType(..) )
import Prelude
import System.Exit
import System.IO
import System.IO.Error (mkIOError, ioeSetErrorString)
import System.Process

main = do 
  T.putStrLn messageStartTryHell
  readProcess' "runghc" [ "Server.hs" 
                        , "-Wall"
                        --, "-Werror"
                        --, "-threaded"
                        --, "-O2"
                        ] [] 
    -- Increase warnings from GHC. 

-- | A copy of System.Process.forkWait (module version 1.1.0.1)
forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

-- | This is a modified version of System.Process.readProcess, so to debug 
-- this (especially if you're not running GHC, please refer to that original 
-- module's source code. This basically contains one function (readProcess') 
-- whose sole function is to fire the assembled app in app/. 
readProcess'
    :: FilePath   -- ^ Filename of the executable (see 'proc' for details)
    -> [String]   -- ^ any arguments
    -> String     -- ^ standard input
    -> IO String  -- ^ stdout
readProcess' cmd args input =
    mask $ \restore -> do
      (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit,
                                       
                                       cwd = Just "./app/"
                                          -- This is the only line Doof adds to
                                          -- the original (readProcess).
                                       }
      flip onException
        (do hClose inh; hClose outh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming the output
        output  <- hGetContents outh
        waitOut <- forkWait $ evaluate $ rnf output

        -- now write and flush any input
        when (not (null input)) $ do hPutStr inh input; hFlush inh
        hClose inh -- done with stdin

        -- wait on the output
        waitOut
        hClose outh

        -- wait on the process
        ex <- waitForProcess pid

        case ex of
         ExitSuccess   -> return output
         ExitFailure r ->
          ioError (mkIOError OtherError ("readProcess: " ++ cmd ++
                                         ' ':unwords (map show args) ++
                                         " (exit " ++ show r ++ ")")
                                     Nothing Nothing)
