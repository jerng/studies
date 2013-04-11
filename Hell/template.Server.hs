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
app = \request-> renderRep $ getRep request 

-- | Beware that the key should be stored somewhere outside ./app, otherwise
-- it will have to be changed every time MakeHell.main is run.
getRep :: Request -> ResourceT IO Report
getRep req = do
  key <- lift {- into ResourceT -} getDefaultKey
  let initialRep = defaultReport 
        { request = Just req

        , session = 
            case sessionValue.onlyCookies.requestHeaders...req of
              Nothing           ->   ["session":= String "no cookie data"]
              Just cookieValue  -> 

                case decrypt key cookieValue of
                  Nothing -> ["session":= String "undecryptable"]
                  Just decrypted -> decdoc decrypted

        } 
      (rep,act) = confirmAct $ router initialRep 
  lift {- into ResourceT -} $ return {- into IO -} $ applyActToRep act rep

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

confirmAct :: Report -> (Report,Action)
confirmAct rep = 
  let aR = actRoute rep
  in  if aR == Hell.Lib.staticFileRoute
      then (rep { static = True },\_->rep { static = True }) -- CRUFTY!
      else case getAct aR of
        Just act  -> (rep, act)
        Nothing -> (rep { actRoute = Hell.Lib.noSuchActionRoute
                        , meta =  tConcat 
                          [ meta rep , Hell.Lib.metaNoSuchAction ]
                        }
                   , fromJust $ getAct (Hell.Lib.noSuchActionRoute)
                   ) 
                      -- Perhaps unnecessarily wordy?
                      -- Does GHC optimise-away messes like this?

applyActToRep :: Action -> Report -> Report
applyActToRep act rep = AppController.main rep act

applyActToSubRep :: Action -> Report -> Report
applyActToSubRep act rep = AppController.subMain rep act

-- | Maybe add a hook from here, to Hell.Conf.
-- Maybe replace this with a regex-style router, as many other frameworks have.
router :: Report -> Report
router rep = case pathInfo $ fromJust $ request rep of
  []        -> rep  { actRoute = Hell.Lib.defaultRoute }
  "":[]     -> rep  { actRoute = Hell.Lib.defaultRoute }
  con:[]    -> rep  { actRoute = (tToLower con, Hell.Lib.indexAction) }
  con:"":x  -> rep  { actRoute = (tToLower con, Hell.Lib.indexAction) }
  con:act:x -> rep  { actRoute = (tToLower con, tToLower act)
                    , pathVars = x
                    }
    
{- for reference:

data Response = 
ResponseFile Status ResponseHeaders FilePath (Maybe FilePart)	 
ResponseBuilder Status ResponseHeaders Builder	 
ResponseSource Status ResponseHeaders (Source (ResourceT IO) (Flush Builder))	

Hell.Server.(renderRep) should have branches to deal with all the above,
eventually.  FilePath, FilePart, Source, etc. should be passed in the
ViewDictionary.  (renderRep) should be able to determine, from the Report,
which data constructor of Response is required by the Controller.

ResponseHeaders are currently hardcoded here as [].
Later, there should be a mechanism for updating the
ResponseHeaders based on the Report from the Action.

-}

-- | Takes Report from a Controller, returns a variety of ResponseBuilder.
--
renderRep :: ResourceT IO Report -> ResourceT IO Response
renderRep rep''' = do
  rep <- rep'''

  -- next line only efficient if lazy, as only used in (else) branch
  reqString <- showRequest $ lift.return.fromJust $ request rep

  if static rep 
    then return $ ResponseFile 
                  (status rep) 
                  (resHeaders rep) 
                  ("./Files/" ++ (tUnpack $ tIntercalate "/" $ pathVars rep) )
                  Nothing
         -- Perhaps, included the ability to render Report { debug } to the
         -- ResponseHeaders; currently debug only happens below.
    else 
      let subRepToText (key,subReport) = 
            key := String (repToText $ applyActToSubRep a subRep)
            where (subRep,a) = confirmAct subReport 
          
          -- Append Report { debug } to Report { meta }.
          rep'' =
            if Hell.Lib.appMode == Production
            then rep
            else do
              rep 
                { meta = tConcat 
                    [ meta rep
                    , tConcat
                      [ "<div class=\"debug\"><h4>Debug\
                      \ (Hell.Conf.appMode == "
                      , tPack.show $ Hell.Lib.appMode
                      , ")</h4><pre><b>debug</b>: "
                      , tIntercalate "<br/><b>debug</b>: " $

                        -- Finalise Report {debug} here. (?) 
                        -- (After this, changes won't be output to View.)
                          if Hell.Lib.appMode > Development1
                          then reverse.debug...rep
                          else  
                              tAppend (tPack reqString) "<br/>"
                            : tAppend "session<br/>" (showDoc 0.session...rep)
                            : reverse.debug...rep

                      , "</pre></div>"
                      ]  
                    ]
                }

          -- Render sub-reports, if any exist.
          rep' = case subReports rep'' of 
            [] -> rep''
            subReps -> rep''
              { subReports = []
              , viewBson = 
                (viewBson rep'') ++ (map subRepToText subReps)
              }
      in  do
          headers <- getResHeaders rep'

          return $ ResponseBuilder (status rep') headers $ 
            -- SOFTEN CODE HERE: there are other types of ResponseBuilders
            fromText $ 
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
                    -- TODO: soften these arguments.
                }

getResHeaders :: Report -> ResourceT IO [Header]
getResHeaders rep = do
  key <- lift getDefaultKey
  encrypted <- lift.encryptIO key.encdoc... 
    --session rep
    ["key":= String "value","key2":=Doc["key3":=String"value2","key4":=Int32 324]]
  lift $ return $ concat 
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
  (fromJust $ getView Hell.Lib.noSuchViewRoute)
  (getView $ viewRoute r)
  r

-- | SHOULD THIS GO INTO (Hell.Conf) ?

{- TO CONSULT PROFESSIONALS:
 Getting some strange compiler behaviour here. The code:
    do return $ head appControllerVariables
 returns [appControllerVariables] instead of appControllerVariables.

 Using Text data as keys. Using ADTs revealed complications. That may be 
 reattempted in the future.
-}

getAct :: Route -> Maybe Action
getAct r 
  {-makeHell:ListActions-}
  | _ <- r = Nothing

getView :: Route -> Maybe (Report -> Text)
getView r 
  {-makeHell:ListViews-}
  | _ <- r = Nothing
