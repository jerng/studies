{-# LANGUAGE OverloadedStrings #-} 

import Hell.Lib

import qualified AppController

{-makeHell:ImportControllers-}

{-makeHell:ImportViews-}

main :: IO ()
main =  run 
        hellServerPort 
        -- $ logStdoutDev 
        app

app :: Request -> ResourceT IO Response 
app = \req-> do
  key <- lift getDefaultKey
  iv <- lift randomIV 
  let finalRep = actOnRep $ confirmAct $ actRouter $ initSession key req 
  reqString <- showRequest $ lift.return.fromJust $ request finalRep 
  lift.return $ respond finalRep reqString key iv

-- | The key file should be stored somewhere outside ./app, otherwise
-- it is deleted every time MakeHell.main is run.
initSession :: Key -> Request -> Report
initSession key req = defaultReport 
  { request = Just req
  , session = 
      case sessionValue.onlyCookies.requestHeaders...req of
        Nothing           ->   ["session":= String "no cookie data"]
        Just cookieValue  -> 
          case decrypt key cookieValue of
            Nothing         -> ["session":= String "undecryptable"]
            Just decrypted  -> decdoc decrypted
  } 

-- | Optimised? Feels crufty.
-- This function is doing a lot: it looks through [Header] to find field-values
-- of the form (Hell.Lib.sessionCookieName ++ "=" ++ encrypted serialised Bson)
-- . Also, even if it remains all here, the implementation might be shortened
-- with a (foldx) so that the input is traversed only once.
sessionValue :: [Header] -> Maybe ByteString
sessionValue headers = 
  let scrub header = 
        let (name,exname) = bsSpan (/='=') $ snd header
        in  if    name    ==  Hell.Lib.sessionCookieName
              &&  exname  /=  "="
              &&  exname  /=  bsEmpty
            then bsTakeWhile (/=';') $ bsTail exname
            else bsEmpty
  in  case filter (/=bsEmpty) $ map scrub headers of
        [] -> Nothing
        value:_ -> Just value

-- | This function sets a Report's (actRoute) based on its (request)'s (pathInfo)
--
--  Maybe add a hook from here, to Hell.Conf.
--  Maybe replace this with a regex-style actRouter, as many other frameworks have.
actRouter :: Report -> Report
actRouter rep = case pathInfo $ fromJust $ request rep of
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
          { actRoute = Hell.Lib.missingActionRoute
          , meta =  tConcat [ meta rep , Hell.Lib.metaNoSuchAction ]
          , action = fromJust.getAct...(Hell.Lib.missingActionRoute)
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
respond :: Report -> String -> Key -> IV -> Response
respond rep reqString key iv = if static rep 
  then respondWithFile rep reqString key iv
  else respondWithBuilder rep reqString key iv

--  | Perhaps, included the ability to render Report { debug } to the
-- ResponseHeaders; currently debug only happens in (respondWithBuilder).
respondWithFile :: Report -> String -> Key -> IV -> Response
respondWithFile rep reqString key iv = 
  ResponseFile status' headers filePath Nothing where
    status'   = status rep
    headers   = getResHeaders rep key iv
    filePath  = "./Files/" ++ (tUnpack $ tIntercalate "/" $ pathVars rep)

-- | I haven't properly studied Builders, but they're probably useless if 
-- used this way. At some point, I guess I'll refactor Hell so that they're
-- utilised properly.
respondWithBuilder :: Report -> String -> Key -> IV -> Response
respondWithBuilder rep reqString key iv = 
  let rep'    = renderSubReps $ renderDebug rep reqString
      headers =  getResHeaders rep' key iv
  in  ResponseBuilder (status rep') headers $ fromText $ 
        case viewTemplate rep of
          Nothing -> repToText rep'
          Just route -> repToText rep' -- rendered outer view
            { viewTemplate = Nothing
            , viewRoute = route
            , viewBson =              -- rendered inner view
                ( Hell.Lib.keyOfTemplatedView := String (repToText rep') )
                -- DEBUG to VIEW: happens here.
              : ( Hell.Lib.keyOfMetaView := String (meta rep') )
              : viewBson rep' 
            }

-- | Append Report { debug } to Report { meta }.
renderDebug :: Report -> String -> Report
renderDebug rep reqString =
  if  Hell.Lib.appMode == Production
  then rep
  else rep 
    { meta = tConcat 
        [ meta rep
        , tConcat
          [ "<span class=\"debug\">Debug\
          \ (Hell.Conf.appMode == "
          , tPack.show $ Hell.Lib.appMode
          , ")</span>"
          , tConcat $ map debugf...
            -- Finalise Report {debug} here. (?) 
            -- (After this, changes won't be output to View.)
              if    Hell.Lib.appMode > Development1
              then  reverse.debug...rep
              else  
                    tAppend (tPack reqString) "<br/>"
                  : tAppend "session<br/>" (showDoc 0.session...rep)
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

getResHeaders :: Report -> Key -> IV -> [Header]
getResHeaders rep key iv = 
  let encrypted = encrypt key iv $ encdoc  
      
      -- | OBVIOUSLY TEMPORARY: DATA FOR THE SESSION
        [ "key":= String "value"
        , "key2":=Doc
          [ "key3":=String"value2"
          , "key4":=Int32 324
          ]
        ]

  in  concat 
      [ resHeaders rep
      , [ ( "Set-Cookie", cookieToBS Hell.Lib.defaultCookie 
            { cookieName = Hell.Lib.sessionCookieName
            , cookieValue = encrypted
  --          , cookiePairs = [ ("path","/")
  --                          , ("Max-Age","0")
  --                          , ("Domain","localhost")
  --                          , ("expires","Thu, 01 Jan 1970 00:00:00 GMT")
  --                          ]
            } 
          ) 
        ]
      ]


-- | Takes Report from a Controller, returns a Text.
repToText :: Report -> Text
repToText r = fromMaybe
  (fromJust $ getView Hell.Lib.missingViewRoute)
  (getView $ viewRoute r)
  r

getAct :: Route -> Maybe Action
getAct r 
  {-makeHell:ListActions-}
  | _ <- r = Nothing

getView :: Route -> Maybe (Report -> Text)
getView r 
  {-makeHell:ListViews-}
  | _ <- r = Nothing
