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

app :: Request -> ResourceT IO Response 
app = \req-> do
  (maybeKey, maybeIv) <-  
        if    Hell.Lib.useEncryption
        then  lift $ do key <- getDefaultKey
                        iv <- randomIV
                        return (Just key, Just iv)
        else  lift.return...(Nothing, Nothing)
  let rep' = Hell.Lib.defaultReport {request = Just req}
      finalRep =  setResCookieHeaders
               .  actOnRep
               .  confirmAct
               .  actRouter
               .  ( if    Hell.Lib.useSessions
                    then  initSession maybeKey maybeIv 
                    else  id
                  )
               .  getReqCookies...rep'

  -- | TODO: Allow per-action sessionless behaviour.
  -- | TODO: Allow per-action cookieless behaviour.
  reqString <- showRequest $ lift.return.fromJust $ request finalRep 
  lift.return.respond...
    if    Hell.Lib.appMode == Production
    then  finalRep
    else  finalRep { shownRequest = reqString }

getReqCookies :: Report -> Report
getReqCookies rep = 
  if    Hell.Lib.useCookies  
  then  rep { reqCookies  = cookieHeadersToKVs
                          . onlyCookieHeaders
                          . requestHeaders
                          . fromMaybe (error "getReqCookies: no request") 
                          . request...rep
            }
  else  rep

-- | The key file should be stored somewhere outside ./app, otherwise
-- it is deleted every time MakeHell.main is run.
initSession :: Maybe Key -> Maybe IV -> Report -> Report
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
                (fromMaybe (error "initSession: no encryption key") maybeKey) 
                cookieValue of
            Nothing         -> Hell.Lib.undecryptableSession
            Just decrypted  -> 
              let decoded = decdoc decrypted
              in  if    decoded == Hell.Lib.undecryptableSession
                  then  Hell.Lib.defaultSession
                  else  decoded 
  } 

-- | This function sets a Report's (actRoute) based on its (request)'s (pathInfo)
--
--  Maybe add a hook from here, to Hell.Conf.
--  Maybe replace this with a regex-style actRouter, as many other frameworks have.
actRouter :: Report -> Report
actRouter rep = case pathInfo.fromJust.request...rep of
  []        -> rep  { actRoute = Hell.Lib.defaultRoute }
  "":[]     -> rep  { actRoute = Hell.Lib.defaultRoute }
  con:[]    -> rep  { actRoute = (tToLower con, Hell.Lib.indexAction) }
  con:"":x  -> rep  { actRoute = (tToLower con, Hell.Lib.indexAction) }
  con:act:x -> rep  { actRoute = (tToLower con, tToLower act)
                    , pathVars = x
                    }

-- | This function sets a Report's (action) based on its (actRoute)
--
-- i)   Requests for static files are acknowledged here.
-- ii)  The Action's Route is checked against the list of existing Actions.
-- iii)  If that Action isn't found, the Report is rerouted to a fail page.
confirmAct :: Report -> Report
confirmAct rep = 
  let aR = actRoute rep
  in  if    aR == Hell.Lib.staticFileRoute
      then  rep { static = True }
      else  case getAct aR of
        Just act  -> rep { action = act }
        Nothing   -> rep 
          { actRoute  = Hell.Lib.missingActionRoute
          , meta      = tConcat [ meta rep , Hell.Lib.metaNoSuchAction ]
          , action    = fromJust.getAct...(Hell.Lib.missingActionRoute)
          }
          -- Perhaps unnecessarily wordy?
          -- Does GHC optimise-away messes like this?

-- ****************************************************************************
-- These two functions have been abstracted and grouped together, because their
--  uses are rather similar, whereas they are called in very different places.
--
actOnRep :: Report -> Report
actOnRep rep = AppController.main rep

actOnSubRep :: Report -> Report
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
respondWithFile :: Report -> Response
respondWithFile rep = 
  ResponseFile status' headers filePath Nothing where
    status'   = status rep
    headers   = getResHeaders rep 
    filePath  = "./Files/" ++ (tUnpack $ tIntercalate "/" $ pathVars rep)

-- | I haven't properly studied Builders, but they're probably useless if 
-- used this way. At some point, I guess I'll refactor Hell so that they're
-- utilised properly.
respondWithBuilder :: Report -> Response
respondWithBuilder rep = 
  let reqString = shownRequest rep
      rep'      = renderSubReps $ renderDebug rep
      headers   =  getResHeaders rep'
  in  ResponseBuilder (status rep') headers $ fromText $ 
        case viewTemplate rep of
          Nothing     -> repToText rep'
          Just route  -> repToText rep' -- rendered outer view
            { viewTemplate = Nothing
            , viewRoute = route
            , viewBson =              -- rendered inner view
                ( Hell.Lib.keyOfTemplatedView := String (repToText rep') )
                -- DEBUG to VIEW: happens here.
              : ( Hell.Lib.keyOfMetaView := String (meta rep') )
              : viewBson rep' 
            }

-- | Append Report { debug } to Report { meta }.
renderDebug :: Report -> Report
renderDebug rep =
  if  Hell.Lib.appMode == Production
  then rep
  else rep 
    { meta = tConcat 
        [ meta rep
        , tConcat
          [ "<span class=\"debug\">Debug\
          \ (Hell.Conf.appMode == "
          , tPack.show...Hell.Lib.appMode
          , ")</span>"
          , tConcat $ map debugf $
            -- Finalise Report {debug} here. (?) 
            -- (After this, changes won't be output to View.)
              if    Hell.Lib.appMode > Development1
              then  reverse.debug...rep
              else  
                    tAppend (tPack.shownRequest...rep) "<br/>"
                  : ( tIntercalate "<br/>  " $ "request cookies" :
                      
                      ( --- MOVE to Hell.Lib.showCookie or Show instance
                        map (\(k,v)-> tConcat 
                              [ tPack.show...k, " : " , tPack.show...v] ) $
                        reqCookies rep
                      )
                    )
                  : tAppend "session<br/>" (showDoc 0 $ session rep)
                  : reverse.debug...rep
          ]  
        ]
    }

-- | Render sub-reports, if any exist.
renderSubReps :: Report -> Report
renderSubReps rep = 
  let subRepToText (key,subReport) = 
        key := String (repToText $ actOnSubRep subRep)
        where subRep = confirmAct subReport 
  in  case subReports rep of 
        []      -> rep
        subReps -> rep
          { subReports = []
          , viewBson = 
            (viewBson rep) ++ (map subRepToText subReps)
          }

getResHeaders :: Report -> [Header]
getResHeaders rep = 
  let maybeKey  = key rep
      maybeIv   = iv rep
      key'      = if    isJust maybeKey
                  then  fromJust maybeKey
                  else  error "getResHeaders: report's key is Nothing"
      iv'       = if    isJust maybeIv
                  then  fromJust maybeIv
                  else  error "getResHeaders: report's iv is Nothing"
      encrypted = encrypt key' iv' $ encdoc $ session rep

  in  concat 
      [ resHeaders rep
      , [ cookieToHeader Hell.Lib.defaultCookie 
            { cookieName = Hell.Lib.sessionCookieName
            , cookieValue = encrypted
  --          , cookiePairs = [ ("path","/")
  --                          , ("Max-Age","0")
  --                          , ("Domain","localhost")
  --                          , ("expires","Thu, 01 Jan 1970 00:00:00 GMT")
  --                          ]
            } 
        ]
      ]

setResCookieHeaders :: Report -> Report
setResCookieHeaders rep = rep 
  { resHeaders = resHeaders rep ++ map cookieToHeader (resCookies rep) }

-- | Takes Report from a Controller, returns a Text.
repToText :: Report -> Text
repToText r = fromMaybe
  (fromJust $ getView Hell.Lib.missingViewRoute)
  (getView  $ viewRoute r)
  r

getAct :: Route -> Maybe Action
getAct r 
  {-makeHell:ListActions-}
  | _ <- r = Nothing

getView :: Route -> Maybe (Report -> Text)
getView r 
  {-makeHell:ListViews-}
  | _ <- r = Nothing
