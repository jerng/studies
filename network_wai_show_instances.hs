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
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import Data.Conduit (Source,Sink,yield,await,($$))
import qualified Data.Vault as V (Vault(..),empty)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.URI (Query)
import Network.HTTP.Types.Version (HttpVersion(..))
import Network.Socket.Internal (SockAddr(..))
import Network.Wai (RequestBodyLength(..),Request(..),Response(..))

main :: IO ()
main = putStrLnTest

putStrLnTest :: IO ()
putStrLnTest = do
  a <- runResourceT.showTest $ _test
  putStrLn a

-- ***************************************************************************
-- REQUEST
--
data Test = Test 
  { requestMethod' :: Method
  , httpVersion' :: HttpVersion
  , rawPathInfo' :: ByteString
  , rawQueryString' :: ByteString  
  , serverName' :: ByteString
  , serverPort' :: Int
  , requestHeaders' :: RequestHeaders
  , isSecure' :: Bool
  , remoteHost' :: SockAddr
  , pathInfo' :: [Text]
  , queryString' :: Query
  , vault' :: V.Vault
  , requestBody' :: Source (ResourceT IO) ByteString
  , requestBodyLength' :: RequestBodyLength
  }

_test :: ResourceT IO Request 
_test = lift.return $ Request 
  { requestMethod = "GET" 
  , httpVersion = HttpVersion 1 0  
  , rawPathInfo = "/rawPathInfo"
  , rawQueryString = "?queryString"
  , serverName = "serverName"
  , serverPort = 3000
  , requestHeaders = [("HeaderName","HeaderValue")]
  , isSecure = True
  , remoteHost = SockAddrUnix "a :: SockAddrUnix String"
  , pathInfo = ["pathInfo1","pathInfo2"]
  , queryString= [("queryItemKey",Just "queryItemValue")]
  , vault = V.empty
  , requestBody = source
  , requestBodyLength = ChunkedBody
  }

showTest :: ResourceT IO Request -> ResourceT IO String
showTest resIOt = do
  t <- lift.runResourceT $ resIOt -- the test data
  
  let requestMethod'' = show.requestMethod $ t
      httpVersion'' = show.httpVersion $ t
      rawPathInfo'' = show.rawPathInfo $ t
      rawQueryString'' = show.rawQueryString $ t
      serverName'' = show.serverName $ t
      serverPort'' = show.serverPort $ t
      requestHeaders'' = show.requestHeaders $ t
      isSecure'' = show.isSecure $ t
      remoteHost'' = show.remoteHost $ t
      pathInfo'' = show.pathInfo $ t
      queryString'' = show.queryString $ t
  requestBody'' <- lift.runResourceT.showResIO $ (requestBody t $$ sink)
  let vault'' = show.vault $ t
      requestBodyLength'' = show.requestBodyLength $ t

  lift.return $ concat 
    [ "Request {", "\n  "
    , "httpVersion = ", httpVersion'', ", ", "\n  "
    , "isSecure = ", isSecure'', ", " , "\n  "
    , "pathInfo = ", pathInfo'', ", ", "\n  "
    , "queryString = ", queryString'', ", ", "\n  "
    , "rawPathInfo = ", rawPathInfo'', ", " , "\n  "
    , "rawQueryString = ", rawQueryString'', ", ", "\n  "
    , "remoteHost = ", remoteHost'', ", ", "\n  "
    , "requestBody = ", requestBody'', ", ", "\n  "
    , "requestBodyLength = ", requestBodyLength'', ", ", "\n  "
    , "requestHeaders = ", requestHeaders'', ", ", "\n  "
    , "requestMethod = ", requestMethod'', ", ", "\n  "
    , "serverName = ", serverName'', ", " , "\n  "
    , "serverPort = ", serverPort'', ", ", "\n  "
    , "vault = ", vault'', ", ", "\n"
    , "}"
    ]
--
-- ***************************************************************************

source :: Source (ResourceT IO) ByteString
source = do
  yield "requestBody"

sink :: Sink ByteString (ResourceT IO) ByteString
sink = do
  a :: (Maybe ByteString) <- await
  case a of
    Nothing -> return ""
    Just b -> return b

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
