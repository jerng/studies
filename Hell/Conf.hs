{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hell.Conf 
  ( module Hell.Types
  , warpServer
  , appMode
  , appWebRoot
  , useCookies
  , useEncryption
  , useSessions
  , defaultSession
  , undecryptableSession
  , defaultReport
  , defaultCookieName
  , sessionCookieName
  , defaultCookie
  , metaNoSuchAction
  , keyOfMetaView
  , keyOfTemplatedView
  , defaultViewTemplate
  , defaultHeaders
  , defaultStatus
  , indexAction
  , defaultRoute
  , missingViewRoute
  , staticFileRoute
  , ViewExp (..)
  , parseRequestBodyBackEnd
  , missingActionRoute
  , controllerImports
  , viewImports
  ) where

import Data.Bson (Document,Field(..),Value(..)) 
import qualified Data.ByteString as BS (ByteString) 
import qualified Data.ByteString.Lazy as LBS
import Data.List ()
import qualified Data.Text as T (pack,Text,append,intercalate)
import Hell.Types
import Network.HTTP.Types (Status,accepted202) 
import Network.HTTP.Types.Header (hContentType,Header)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings,defaultSettings)
import Network.Wai.Parse (BackEnd,lbsBackEnd)

-- Network.Wai.Handler.Warp.defaultSettings { settingsPort is 3000 }
--
-- If you intend to keep a state in the web server across multiple
-- request-response pairs, this page is informative:
-- https://github.com/yesodweb/yesod/wiki/Keeping-(in-memory)-state-with-warp
--
-- Some items which probably ought to be cached in the server include the
-- (Web.ClientSession) key, etc. I have no idea how Warp handles it if 
-- things like these are not coded outside the application, but are redundant
-- across all requests/responses.
--
warpServer :: Application -> IO ()
warpServer app = runSettings defaultSettings app

appMode :: AppMode
appMode = -- FullAutoDebug 
          -- SemiAutoDebug
           ManualDebug
          -- Production

-- Path segments before the /controller/action;
-- mainly used by Hell.Lib.url
appWebRoot :: T.Text
appWebRoot = ""

{- Not sure that the following are useful.

appHostname :: T.Text
appHostname = "localhost"

appScheme :: T.Text
appScheme = "http"

appPort :: Int
appPort = 3000

-}

-- ****************************************************************************
-- Incompatible settings here should be caught by makeHell at pre-compile time.
-- FIXME- : THEY CURRENTLY ARE NOT... are they?
useEncryption :: Bool
useEncryption = True

-- Implemented as a softer switch: if useCookies=True and useSession=False
-- then the session cookie's value will be set to "deleted"
useSessions :: Bool
useSessions = True 

-- Implemented as a harder switch: simply stops adding Request {resCookies} to
-- Request {resHeaders}
useCookies :: Bool
useCookies = True
--
-- ****************************************************************************

defaultSession :: Document 
defaultSession = ["data" := Null] 

-- | This is probably a temporary solution/mechanism
undecryptableSession :: Document 
undecryptableSession =  ["error" := String "undecryptable"]

-- I would really like to know how all this setting of defaults
-- affects memory use. Testing will be required.
defaultReport :: Report
defaultReport = Report  { request = Nothing
                        , params = []
                        , files = []
                        , form_ = []
                        , data_ = [] 
                        , shownRequest = ""
                        , key = Nothing
                        , iv = Nothing
                        , session = defaultSession 
                        , actRoute = defaultRoute
                        , viewRoute = defaultRoute
                        , viewData_ = []
                        , subReports = []
                        , status = defaultStatus 
                        , reqCookies = []
                        , resCookies = []
                        , resHeaders = defaultHeaders
                        , viewTemplateRoute = defaultViewTemplate
                        , pathVars = []
                        , static = False
                        , debug = []
                        , action = id 
                        }

defaultCookieName :: BS.ByteString
defaultCookieName = "Hell"

sessionCookieName :: BS.ByteString
sessionCookieName = "FsbD"

defaultCookie :: Cookie -- CHANGE THIS!!!
defaultCookie = Cookie  { cookieName = defaultCookieName
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

metaNoSuchAction :: T.Text
metaNoSuchAction = "We could not find the page that you are looking for." 
  `T.append` if appMode == Production then "" else 
    "<div class=\"debug\">Hell.Server.confirmAction did not find the\
    \ requested action in Hell.Server.actionList. This is the list assembled\
    \ by ./makeHell.hs and spliced into ./app/Server.hs.</div>"

keyOfMetaView :: T.Text
keyOfMetaView = "metaView"

-- | The key in a viewData_ of a view template, whose value is the text 
-- of a view that has been full rendered, along with any of its subviews. 
-- (Report and sub Reports) by Hell.Server.renderReport.
keyOfTemplatedView :: T.Text
keyOfTemplatedView = "viewContent"

defaultViewTemplate :: Maybe Route
defaultViewTemplate = Just ("default","template")

defaultHeaders :: [Header]
defaultHeaders = 
  [ (hContentType,"text/html; charset=utf-8")
  ]

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

class ViewExp a where toText :: a -> T.Text
instance ViewExp () where toText a = "\"()\"" 
instance ViewExp Int where toText a = T.pack $ show a
instance ViewExp Double where toText a = T.pack $ show a
instance ViewExp T.Text where toText a = a
instance ViewExp [Int] where toText a = T.pack $ show a

-- DO NOT CHANGE THIS NAIVELY.
-- If you change the BackEnd, you may have to change the BackEnd type argument.
-- Also, nothing but (lbsBackEnd) is currently supported.
-- So it's only useful in the future.
parseRequestBodyBackEnd :: BackEnd 
                              LBS.ByteString
                              --FilePath
parseRequestBodyBackEnd = lbsBackEnd
                          -- tempFileBackEnd
                          -- tempFileBackEndOpts

controllerImports :: T.Text
controllerImports = T.intercalate "\n"
  [ "import Data.Bson (Document, Field (..), Value (..))"
  , "import qualified Data.Text as T"
  , "import Network.HTTP.Types (ok200)"
  ]

viewImports :: T.Text
viewImports = T.intercalate "\n"
  [ "import Data.Bson (Document,lookup,Val,Label)"
  , "import Data.Maybe"
  , "import qualified Data.Text as T"
  , "import Debug.Trace"
  ]
