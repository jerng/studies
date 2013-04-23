{-# LANGUAGE OverloadedStrings #-} 

module Main (main) where

import Blaze.ByteString.Builder.Char.Utf8 (fromText)
--import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT,ResourceT)
import Data.Bson as Bson (Document,Field(..),Value(..),merge,(!?))
import qualified Data.ByteString.Lazy.Char8 as LBS (fromChunks)
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceLbs)
import Data.Maybe
import qualified Data.Text as T
import qualified Hell.Conf
import Hell.Lib
import Hell.Parse.Forms
import Hell.Parse.Headers
import Hell.Show
import Hell.Types
-- import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types (found302)
import Network.Wai (RequestBodyLength(..),Request(..),Response(..))
import Network.Wai.Parse (parseRequestBody)
import Web.ClientSession (randomIV, encrypt, encryptIO, decrypt, getDefaultKey)

import qualified AppController

{-makeHell:ImportControllers-}

{-makeHell:ImportViews-}


main :: IO ()
main =  Hell.Conf.warpServer
        -- $ logStdoutDev 
        app

-- |  TODO: Allow per-action sessionless behaviour.
--    TODO: Allow per-action cookieless behaviour.
app :: Request -> ResourceT IO Response 
app = \req-> do
  
  -- We'll use this flag a couple of times below.
  let post = "POST" == requestMethod req

  -- Sink the requestBody, and use it to construct multiple Sources later
  reqBod <-
    if    post 
    then  lift.runResourceT $ (requestBody req $$ sinkForRequestBody)
    else  lift.return $ "" -- Not sure if this is safer than "ignored"

  -- Get stuff for use in pure code.
  reqString <- 
    if    Hell.Conf.appMode == Production
    then  lift.return $ ""
    else  showRequest reqBod . lift . return $ req 

  -- Get stuff for use in pure code.
  (maybeKey, maybeIv) <-  
    if    not Hell.Conf.useEncryption
    then  lift.return $ (Nothing, Nothing)
    else  lift $ do 
            key <- getDefaultKey
                  -- This method generates & uses ./app/client_session_key.aes
                  -- Alternatively, this could be stored in Hell.Conf, like
                  -- CakePHP 1.2 did, I suppose.
            iv <- randomIV
                  -- This method generates a new IV upon every request. 
                  -- Alternatively, this could be stored in Hell.Conf, like
                  -- CakePHP 1.2 did, I suppose.
            return (Just key, Just iv)
  
  -- Get stuff for use in pure code.
  (params', files') <-
    if    not post
    then  lift.return $ ([],[])
    else  parseRequestBody 
          Hell.Conf.parseRequestBodyBackEnd
          req { requestBody = sourceLbs $ LBS.fromChunks [reqBod] }
                -- Since we sank the source, above, here we must recreate it 
                -- if "it" is to be used again. (-_- wtf mutable data)

  let
      -- This is the initialised Report - pass it down the pipeline...
      rep' =  Hell.Conf.defaultReport 
              { request = Just req  
                { requestBody = sourceLbs $ LBS.fromChunks [reqBod] }
                -- Since we sank the source, above, here we must recreate it 
                -- if "it" is to be used again. (-_- wtf mutable data)
              , key = maybeKey
              , iv = maybeIv
              , params = params'
              , files = files'
              } 

      -- The pipeline:
      finalRep =  
        setResCookieHeaders
        .setResSessionCookie
        .actOnRep
          -- Within the Action, further authorisations may happen.
        -- . authorisationHere (in AppController?) 
          --  (depends on Authentication & details of Request)
        .populateData
        .populateForm
        .confirmAct
        .actRouter
          -- Based on normalised path variables
        -- . authenticationHere (depends only on Session)
        .( if Hell.Conf.useSessions then initSession else id)
        .getReqCookies $ rep'

  lift.return.respond $
    if    Hell.Conf.appMode == Production
    then  finalRep
    else  finalRep { shownRequest = reqString }

getReqCookies :: ReportHandler
getReqCookies rep = 
  if    not Hell.Conf.useCookies  
  then  rep
  else  rep { reqCookies  = cookieHeadersToKVs
                          .onlyCookieHeaders
                          .requestHeaders
                          .fromMaybe (error "getReqCookies: no Request") 
                          .request $ rep
            }

-- | The key file should be stored somewhere outside ./app, otherwise
-- it is deleted every time MakeHell.main is run.
                -- If we want to (error) when Conf.useSession is true,
                -- but when the (key) or (iv) is not obtained, here would 
                -- be the place to do it.
initSession :: ReportHandler
initSession rep = rep 
  { session = 
      case lookup Hell.Conf.sessionCookieName $ reqCookies rep of
      Nothing           -> Hell.Conf.defaultSession 
      Just cookieValue  ->
        case  decrypt 
              (fromMaybe (error "initSession: no encryption Key") $ key rep) 
              cookieValue of
        Nothing         -> Hell.Conf.undecryptableSession
        Just decrypted  -> 
          let decoded = decdoc decrypted
          in  if    decoded == Hell.Conf.undecryptableSession
              then  Hell.Conf.defaultSession
              else  decoded 
  } 

-- | This function sets a Report's (actRoute) based on its (request)'s (pathInfo)
--  Maybe add a hook from here, to Hell.Conf.
--  Maybe replace this with a regex-style actRouter, as many other frameworks have.
actRouter :: ReportHandler
actRouter rep = case pathInfo.fromMaybe (error "actRouter: no Request").request $ rep of
  []        -> rep  { actRoute = Hell.Conf.defaultRoute }
  "":[]     -> rep  { actRoute = Hell.Conf.defaultRoute }
  con:[]    -> rep  { actRoute = (T.toLower con, Hell.Conf.indexAction) }
  con:"":x  -> rep  { actRoute = (T.toLower con, Hell.Conf.indexAction) }
  con:act:x -> rep  { actRoute = (T.toLower con, T.toLower act)
                    , pathVars = x
                    }

-- | This function sets a Report's (action) based on its (actRoute)
-- i)   Requests for static files are acknowledged here.
-- ii)  The Action's Route is checked against the list of existing Actions.
-- iii)  If that Action isn't found, the Report is rerouted to a fail page.
confirmAct :: ReportHandler
confirmAct rep = 
  let aR = actRoute rep
  in  if    aR == Hell.Conf.staticFileRoute
      then  rep { static = True }
      else  case getAct aR of
        Just act  -> rep 
          { action = act
          , viewRoute = aR
          }
        Nothing   -> rep 
          { actRoute  = Hell.Conf.missingActionRoute
          , viewRoute = Hell.Conf.missingActionRoute
          , session   = merge 
                        ["meta":=String Hell.Conf.metaNoSuchAction ] $ 
                        session rep
          , action    = fromMaybe 
                        (error "confirmAct: Hell.Conf.missingActionRoute is\
                        \ itself missing. Shucks.") $
                        getAct (Hell.Conf.missingActionRoute)
          }
          -- Perhaps unnecessarily wordy?
          -- Does GHC optimise-away messes like this?

populateForm :: ReportHandler
populateForm rep = rep 
  { form_ = 
    [ "temp" := Array
      (
        let req = fromMaybe (error "populateForm: no Request") $ request rep
            paramToQuery = \(n,v)->(n,Just v)
        in  ( mapMaybe queryItem'ToMaybeValue $
                case requestMethod req of
                "GET"   -> queryString req
                "POST"  -> map paramToQuery $ params rep
            ) ++
            ( mapMaybe file'ToMaybeValue $ files rep )
      )
    ]
  }

populateData :: ReportHandler
populateData rep = rep { data_ = merge data_' $ data_ rep} where 
  data_' = 
    [ "TEMP (until Request {data_} is normalised from forms to models)" := Doc 
      [ "path" := ( Array $ map String $ pathVars rep ) 
      , "form" := ( Doc $ form_ rep )
      , "TODO" := String "debugging some of these above ^^"
      ]
    ]

-- ****************************************************************************
-- These two functions have been abstracted and grouped together, because their
--  uses are rather similar, whereas they are called in very different places.
--
actOnRep :: ReportHandler
actOnRep rep = AppController.main rep

actOnSubRep :: ReportHandler
actOnSubRep rep = AppController.subMain rep
--
-- ****************************************************************************

{- for reference:
data Response = 
  ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)	 
  ResponseBuilder Status ResponseHeaders Builder	 
  ResponseSource Status ResponseHeaders (Source (ResourceT IO) (Flush Builder))	
Hell.Server.(respond) should have branches to deal with all the above,
eventually.  FilePath, FilePart, Source, etc. should be passed in the
ViewBson.  (respond) should be able to determine, from the Report,
which data constructor of Response is required by the Controller.

-}

-- | Takes Report from a Controller, returns a variety of ResponseBuilder.
respond :: Report -> Response
respond rep = ( if    static rep 
                then  respondWithFile
                else  respondWithBuilder ) rep

--  | Perhaps, included the ability to render Report { debug } to the
-- ResponseHeaders; currently debug only happens in (respondWithBuilder).
-- Does not send cookies with files; not sure if this is ideal.
-- CHECK: also not sending headers. Is this ok?
respondWithFile :: Report -> Response
respondWithFile rep = 
  ResponseFile status' headers filePath Nothing 
  where
    status'   = status rep
    headers   = []--resHeaders rep 
    filePath  = "./Files/" ++ (T.unpack $ T.intercalate "/" $ pathVars rep)

-- | I haven't properly studied Builders, but they're probably useless if 
-- used this way. At some point, I guess I'll refactor Hell so that they're
-- utilised properly.
respondWithBuilder :: Report -> Response
respondWithBuilder rep = let { s = status rep } in
  if    s == found302
  then  ResponseBuilder found302 (resHeaders rep) $ fromText ""
        -- CONSIDER: should a body of "click here if you are 
        --  not redirected etc." be included?
  else
    let reqString = shownRequest rep
        rep'      = renderSubReps rep
        headers   = resHeaders rep'
    in  ResponseBuilder (status rep') headers $ fromText $  
          case viewTemplateRoute rep of
            Nothing     -> repToText rep'
            Just route  -> repToText rep' 
              -- rendered template wrapper ("outer view") 
              { viewTemplateRoute = Nothing
              , viewRoute = route
              , viewBson = flip merge (viewBson rep')            

                [ Hell.Conf.keyOfTemplatedView := 
                    String ( repToText rep' { debug=[], shownRequest="" } )
                  -- rendered template contents ("inner view")
                , Hell.Conf.keyOfMetaView := 
                    maybe Null String ( session rep !? "meta" )
                ]

              }

-- | Most of this should probably go into the CSS file, with :before, etc.
renderDebug :: Report -> T.Text
renderDebug rep =
  if  Hell.Conf.appMode == Production
  then ""
  else T.concat
    [ "<hr/><b class=\"debug\">Debug in viewRoute "
    , T.pack.show.viewRoute$rep, " (Hell.Conf.appMode == "
    , T.pack.show$Hell.Conf.appMode , ")</b>"
    , T.concat $ map debugf $
      -- Finalise Report {debug} here. (?) 
      -- (After this, changes won't be output to View.)
        if    Hell.Conf.appMode > SemiAutoDebug
        then  reverse.debug$rep
        else  T.append (T.pack.shownRequest$rep) "<br/>"
              : ( T.intercalate "<br/>  " $ 
                  "Request {reqCookies}:" 
                  : ( map 
                      (\(k,v)-> T.concat [ T.pack.show$k
                                        , " : " 
                                        , T.pack.show$v
                                        ] 
                      ) $ reqCookies rep 
                    ) --- MOVE to Hell.Lib.showCookie or Show instance
                )
              : T.append "Request {session}:" (showDoc False 0 $ session rep)
--              : ( T.intercalate "<br/>  " $ 
--                  "Request {postQuery}:" 
--                  : ( map (T.pack.show) $ postQuery rep )
--                )
              : ( reverse.debug $ rep )
    ]  

-- | Render sub-reports, if any exist.
renderSubReps :: ReportHandler
renderSubReps rep = 
  let subRepToText (key,subReport) = 
        key := String ( repToText
                      .renderSubReps
                      .actOnSubRep
                      .confirmAct$subReport
                      )
  in  case subReports rep of 
        []      -> rep
        subReps -> rep
          { viewBson = (viewBson rep) ++ (map subRepToText subReps) }

-- Explore difference between (concat) and (++) here
setResSessionCookie :: ReportHandler 
setResSessionCookie rep = rep
  { resCookies = flip (:) (resCookies rep) 
      ( Hell.Conf.defaultCookie 
        { cookieName = Hell.Conf.sessionCookieName
        , cookieValue =
          if    not Hell.Conf.useSessions
          then  "deleted" -- TODO: also set negative age, etc.?
          else  let maybeKey  = key rep
                    maybeIv   = iv rep
                    key'      = fromMaybe 
                                (error "setResSessionCookie:\
                                \ Report has no encryption Key")
                                maybeKey
                    iv'       = fromMaybe
                                (error "setResSessionCookie:\
                                \ Report has no encryption IV")
                                maybeIv
                in  encrypt key' iv' $ encdoc $ session rep 

-- | To help devs with testing:
--        , cookiePairs = [ ("path","/")
--                        , ("Max-Age","0")
--                        , ("Domain","localhost")
--                        , ("expires","Thu, 01 Jan 1970 00:00:00 GMT")
--                        ]

        } 
      )
  }

-- Explore difference between (concat) and (++) here
setResCookieHeaders :: ReportHandler
setResCookieHeaders rep = 
  if    not Hell.Conf.useCookies 
  then  rep 
  else  rep
    { resHeaders = resHeaders rep ++ map cookieToHeader (resCookies rep) }

-- | Takes Report from a Controller, returns a Text.
repToText :: Report -> T.Text
repToText r = flip T.append (renderDebug r) $
  ( fromMaybe
    ( fromMaybe 
      (error "repToText: Hell.Conf.missingViewRoute is itself missing. Shucks.") $
      getView Hell.Conf.missingViewRoute
    )
    ( getView $ viewRoute r )
  ) $ viewBson r 

getAct :: Route -> Maybe Action
getAct r 
  {-makeHell:ListActions-}
  | _ <- r = Nothing

getView :: Route -> Maybe (Document -> T.Text)
getView r 
  {-makeHell:ListViews-}
  | _ <- r = Nothing
