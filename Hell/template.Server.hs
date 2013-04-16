{-# LANGUAGE OverloadedStrings #-} 

module Hell.Server () where

import Hell.Lib

import qualified AppController

{-makeHell:ImportControllers-}

{-makeHell:ImportViews-}

main :: IO ()
main =  Hell.Lib.warpServer
        -- $ logStdoutDev 
        app

-- |  TODO: Allow per-action sessionless behaviour.
--    TODO: Allow per-action cookieless behaviour.
app :: Request -> ResourceT IO Response 
app = \req-> do
  let post = "POST" == requestMethod req
  reqBod <-
    if    post 
    then  lift.runResourceT...(requestBody req $$ sinkForRequestBody)
    else  lift.return..."" -- Not sure if this is safer than "ignored"
  (maybeKey, maybeIv) <-  
    if    not Hell.Lib.useEncryption
    then  lift.return...(Nothing, Nothing)
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
  let rep' = Hell.Lib.defaultReport {request = Just req}
      finalRep =  setResCookieHeaders
                . setResSessionCookie
                . actOnRep
                  -- Within the Action, further authorisations may happen.
                
                -- . authorisationHere (in AppController?) 
                  --  (depends on Authentication & details of Request)
                
                . ( if    easyMode 
                    then  populateEasy
                    else  id 
                  )
                . ( if    post
                    then  getPostVars reqBod
                    else  id
                  )
                . confirmAct
                . actRouter
                  -- Based on normalised path variables

                -- . authenticationHere (depends only on Session)
                
                . ( if    Hell.Lib.useSessions
                    then  initSession maybeKey maybeIv 
                    else  id
                  )
               .  getReqCookies...rep'
  reqString <- showRequest $ 
    lift.return.fromMaybe (error "app: no Request") $ request finalRep 
  lift.return.respond...
    if    Hell.Lib.appMode == Production
    then  finalRep
    else  finalRep { shownRequest = reqString }

getReqCookies :: ReportHandler
getReqCookies rep = 
  if    not Hell.Lib.useCookies  
  then  rep
  else  rep { reqCookies  = cookieHeadersToKVs
                          .onlyCookieHeaders
                          .requestHeaders
                          .fromMaybe (error "getReqCookies: no Request") 
                          .request...rep
            }

-- | The key file should be stored somewhere outside ./app, otherwise
-- it is deleted every time MakeHell.main is run.
initSession :: Maybe Key -> Maybe IV -> ReportHandler
initSession maybeKey maybeIv rep = rep 
  { key     = maybeKey
  , iv      = maybeIv
                -- If we want to (error) when Conf.useSession is true,
                -- but when the (key) or (iv) is not obtained, here would 
                -- be the place to do it.
  , session = 
      case lookup Hell.Lib.sessionCookieName $ reqCookies rep of
        Nothing           -> Hell.Lib.defaultSession 
        Just cookieValue  ->
          case  decrypt 
                (fromMaybe (error "initSession: no encryption Key") maybeKey) 
                cookieValue of
            Nothing         -> Hell.Lib.undecryptableSession
            Just decrypted  -> 
              let decoded = decdoc decrypted
              in  if    decoded == Hell.Lib.undecryptableSession
                  then  Hell.Lib.defaultSession
                  else  decoded 
  } 

-- | This function sets a Report's (actRoute) based on its (request)'s (pathInfo)
--  Maybe add a hook from here, to Hell.Conf.
--  Maybe replace this with a regex-style actRouter, as many other frameworks have.
actRouter :: ReportHandler
actRouter rep = case pathInfo.fromMaybe (error "actRouter: no Request").request...rep of
  []        -> rep  { actRoute = Hell.Lib.defaultRoute }
  "":[]     -> rep  { actRoute = Hell.Lib.defaultRoute }
  con:[]    -> rep  { actRoute = (tToLower con, Hell.Lib.indexAction) }
  con:"":x  -> rep  { actRoute = (tToLower con, Hell.Lib.indexAction) }
  con:act:x -> rep  { actRoute = (tToLower con, tToLower act)
                    , pathVars = x
                    }

-- | This function sets a Report's (action) based on its (actRoute)
-- i)   Requests for static files are acknowledged here.
-- ii)  The Action's Route is checked against the list of existing Actions.
-- iii)  If that Action isn't found, the Report is rerouted to a fail page.
confirmAct :: ReportHandler
confirmAct rep = 
  let aR = actRoute rep
  in  if    aR == Hell.Lib.staticFileRoute
      then  rep { static = True }
      else  case getAct aR of
        Just act  -> rep 
          { action = act
          , viewRoute = aR
          }
        Nothing   -> rep 
          { actRoute  = Hell.Lib.missingActionRoute
          , viewRoute = Hell.Lib.missingActionRoute
          , meta      = tConcat [ meta rep , Hell.Lib.metaNoSuchAction ]
          , action    = fromMaybe 
                        (error "confirmAct: Hell.Lib.missingActionRoute is\
                        \ itself missing. Shucks.") $
                        getAct (Hell.Lib.missingActionRoute)
          }
          -- Perhaps unnecessarily wordy?
          -- Does GHC optimise-away messes like this?

-- Use the same rules as Wai does for the Request {pathInfo}, where "key="
-- gets translated to ("key", Just ""), and "key" to ("key",Nothing)
getPostVars :: ByteString -> ReportHandler
getPostVars reqBod rep = rep 
  { postVars = map  ( \bs-> case bsSpan (/='=') bs of
                        -- ("",_)  -> ("",Nothing) -- this should never happen
                        (k,"")  -> (k, Nothing)
                        (k,v)   -> (k, Just $ bsTail v) 
                    ) $ bsSplit '&' reqBod 
  }

populateEasy :: ReportHandler
populateEasy rep = rep { easy = easy'} where 
  easy' = 
    [ "controller"  := String ... fst ... actRoute rep
    , "action"      := String ... snd ... actRoute rep
    , "session"     := Doc ... session rep 
    , "request"     := Doc 
      [ "path" := Array ... map String ... pathVars rep
      , "query" := Doc 
        ... map 
            ( \(bs,maybeBs)-> 
              decodeUtf8 bs := maybe Null ((String).decodeUtf8) maybeBs )
        ... queryString
        ... fromMaybe (error "populateEasy: no Request")
        ... request rep
      , "post" := Doc
        ... map 
            ( \(bs,maybeBs)-> 
              decodeUtf8 bs := maybe Null ((String).decodeUtf8) maybeBs )
        ... postVars rep
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
respondWithFile :: Report -> Response
respondWithFile rep = 
  ResponseFile status' headers filePath Nothing 
  where
    status'   = status rep
    headers   = resHeaders rep 
    filePath  = "./Files/" ++ (tUnpack $ tIntercalate "/" $ pathVars rep)

-- | I haven't properly studied Builders, but they're probably useless if 
-- used this way. At some point, I guess I'll refactor Hell so that they're
-- utilised properly.
respondWithBuilder :: Report -> Response
respondWithBuilder rep = let { s = status rep } in
  if    s == found302
  then  ResponseBuilder found302 (resHeaders rep) $ fromText ""
        -- CONSIDER: should a body of "click here etc." be included?
  else
    let reqString = shownRequest rep
        rep'      = renderSubReps $ renderDebug rep
        headers   = resHeaders rep'
    in  ResponseBuilder (status rep') headers $ fromText $ 
          case viewTemplateRoute rep of
            Nothing     -> repToText rep'
            Just route  -> repToText rep' 
              -- rendered template wrapper ("outer view") 
              { viewTemplateRoute = Nothing
              , viewRoute = route
              , viewBson =              
                  -- rendered template contents ("inner view")
                  ( Hell.Lib.keyOfTemplatedView := String (repToText rep') )
                  -- DEBUG to VIEW: happens here.
                : ( Hell.Lib.keyOfMetaView := String (meta rep') )
                : viewBson rep' 
              }

-- | Append Report { debug } to Report { meta }.
renderDebug :: ReportHandler
renderDebug rep =
  if  Hell.Lib.appMode == Production
  then rep
  else rep 
    { meta = tConcat 
        [ meta rep
        , tConcat
          [ "<span class=\"debug\">Debug (Hell.Conf.appMode == "
          , tPack.show...Hell.Lib.appMode , ")</span>"
          , tConcat $ map debugf $
            -- Finalise Report {debug} here. (?) 
            -- (After this, changes won't be output to View.)
              if    Hell.Lib.appMode > Development1
              then  reverse.debug...rep
              else  tAppend (tPack.shownRequest...rep) "<br/>"
                    : ( tIntercalate "<br/>  " $ 
                        "Request { reqCookies }" 
                        : ( map 
                            (\(k,v)-> tConcat [ tPack.show...k
                                              , " : " 
                                              , tPack.show...v
                                              ] 
                            ) $ reqCookies rep 
                          ) --- MOVE to Hell.Lib.showCookie or Show instance
                      )
                    : tAppend "session<br/>" (showDoc 0 $ session rep)
                    : ( tIntercalate "<br/>  " $ 
                        "Request { postVars }" 
                        : ( map ... tPack.show $ postVars rep )
                      )
                    : reverse.debug...rep
          ]  
        ]
    }

-- | Render sub-reports, if any exist.
renderSubReps :: ReportHandler
renderSubReps rep = 
  let subRepToText (key,subReport) = 
        key := String ( repToText
                      .renderSubReps
                      .renderDebug
                      .actOnSubRep
                      .confirmAct...subReport
                      )
  in  case subReports rep of 
        []      -> rep
        subReps -> rep
          { viewBson = (viewBson rep) ++ (map subRepToText subReps) }

-- Explore difference between (concat) and (++) here
setResSessionCookie :: ReportHandler 
setResSessionCookie rep = rep
  { resCookies = flip (:) (resCookies rep) 
      ( Hell.Lib.defaultCookie 
        { cookieName = Hell.Lib.sessionCookieName
        , cookieValue =
          if    not Hell.Lib.useSessions
          then  "deleted"
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
  if    not Hell.Lib.useCookies 
  then  rep 
  else  rep
    { resHeaders = resHeaders rep ++ map cookieToHeader (resCookies rep) }

-- | Takes Report from a Controller, returns a Text.
repToText :: Report -> Text
repToText r = 
  ( fromMaybe
    ( fromMaybe 
      (error "repToText: Hell.Lib.missingViewRoute is itself missing. Shucks.") $
      getView Hell.Lib.missingViewRoute
    )
    ( getView $ viewRoute r )
  ) $ viewBson r 

getAct :: Route -> Maybe Action
getAct r 
  {-makeHell:ListActions-}
  | _ <- r = Nothing

getView :: Route -> Maybe (Document -> Text)
getView r 
  {-makeHell:ListViews-}
  | _ <- r = Nothing
