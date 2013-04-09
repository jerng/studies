{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , OverloadedStrings
  , ScopedTypeVariables
  #-}

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT,liftResourceT,runResourceT)
import Data.ByteString.Char8 (ByteString)
import Data.Conduit (Source,Sink,yield,await,($$),($=),(=$))
--import Data.Conduit.Internal (ConduitM(..))
import qualified Data.Vault as V (Vault(..),empty)
import Network.Wai (RequestBodyLength(..),Request(..),Response(..))

main :: IO ()
main = putStrLnTest

data Test = Test 
  { requestBodyLength' :: RequestBodyLength
  , vault' :: V.Vault
  , requestBody' :: ResourceT IO ByteString
  }

_test :: ResourceT IO Test
_test = lift.return $ Test 
  { requestBodyLength' = ChunkedBody
  , vault' = V.empty
  , requestBody' = source $$ sink 
  }

putStrLnTest :: IO ()
putStrLnTest = do
  a <- runResourceT.showTest $ _test
  putStrLn a

showTest :: ResourceT IO Test -> ResourceT IO String
showTest resIOt = do
  t <- lift.runResourceT $ resIOt -- the test data
  let requestBodyLength'' = show.requestBodyLength' $ t
      vault'' = show.vault' $ t
  requestBody'' <- lift.runResourceT.showResIO.requestBody' $ t
  lift.return $ concat 
    [ "Test {"
    , "requestBodyLength' = ", requestBodyLength'', ", "
    , "vault' = ", vault'', ", "
    , "requestBody' = ", requestBody'', ","
    , "}"
    ]

--_request = Request 
--  { requestMethod =
--  , httpVersion =
--  , rawPathInfo = 
--  , rawQueryString =
--  , serverName =
--  , serverPort = 
--  , requestHeaders = 
--  , isSecure = 
--  , remoteHost =
--  , pathInfo =
--  , queryString =
--  , requestBody = 
--  , vault = 
--  , requestBodyLength = _requestBodyLength
--  }

source :: Source (ResourceT IO) ByteString
source = do
  yield "requestBody"

sink :: Sink ByteString (ResourceT IO) ByteString
sink = do
  a :: (Maybe ByteString) <- await
  case a of
    Nothing -> return ""
    Just b -> return b

_requestBody :: (ResourceT IO) ByteString
_requestBody = lift $ return "requestBody" 

_vault = V.empty
_requestBodyLength = ChunkedBody 

showResIO :: (Show a) => ResourceT IO a -> ResourceT IO String
showResIO a = do
  b <- lift $ runResourceT a
  lift $ return $ show b

instance Show V.Vault where
  show _ = "a :: Vault"

instance Show RequestBodyLength where
  show r = case r of 
    ChunkedBody -> "ChunkedBody"
    KnownLength w64 -> "KnownLength " ++ show w64

instance Show Request where
  show r = "Hell.Lib instance for Show Request : TODO"

instance Show Response where
  show r = "Hell.Lib instance for Show Response : TODO"

