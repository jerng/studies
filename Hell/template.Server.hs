{-# LANGUAGE OverloadedStrings #-} 

import Hell.Lib

import qualified AppController
import Data.ByteString.Base64 as B64
import Data.ByteString.UTF8 as BS

{-makeHell:ImportControllers-}

{-makeHell:ImportViews-}

main :: IO ()
main = run hellServerPort app

app :: Request -> ResourceT IO Response 
app = \request-> renderRep $ getRep request 

-- ***************************************************************************
-- | UNDER VOLATILE CONSTRUCTION
--
getRep :: Request -> Report
getRep req = 
  let initialRep = defaultReport 
        { request = Just req
        --, session = decdoc "7\STXname\ENQjohn\DLEage\ETB\ETXchild\DC3\STXname\EOTjim"
        , meta = tConcat 
          [ "getRep reporting<br/>"
          , tPack $ show $ requestHeaders req
          , "<br/><br/>debug:<br/>"
          , tPack $ show $ 
            case sessionValue $ onlyCookies $ requestHeaders req of
              Nothing ->   ["session":= String "no data"]
              Just cookieValue -> decdoc $ case B64.decode cookieValue of
                Left s -> BS.fromString s
                Right bs -> bs
          ]

        } 
      (rep,act) = confirmAct $ router initialRep 
  in  applyActToRep act rep
-- ***************************************************************************

-- Optimised? Feels crufty.
sessionValue :: [Header] -> Maybe ByteString
sessionValue hs = 
  let f (_,bs) = 
        let (shn,exshn) = bsSpan (\c->not ('='==c)) bs
        in  if  (shn == Hell.Lib.sessionCookieName) &&
                (not $ exshn == "=") && 
                (not $ exshn == bsEmpty)
            then bsTail exshn
            else bsEmpty
      f' bs'' = not $ bs'' == bsEmpty
  in  case filter f' $ map f hs of
        [] -> Nothing
        shv:_ -> Just shv

-- to Lib?
onlyCookies :: [Header] -> [Header]
onlyCookies headers = filter ((hCookie==).fst) headers

confirmAct :: Report -> (Report,Action)
confirmAct rep = 
  case getAct (routeA rep) of
    Just act  -> (rep, act)
    Nothing -> (rep { routeA = Hell.Lib.noSuchActionRoute
                    , meta =  if    Hell.Lib.appMode == Development 
                              then  tConcat [ meta rep, "<br/>",
                                      Hell.Lib.metaNoSuchAction]
                              else  meta rep
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
router :: Report -> Report
router rep = 
  let rep' = case pathInfo $ fromJust $ request rep of
        []        -> Hell.Lib.defaultRoute
        con:[]    -> ( tToLower con, Hell.Lib.indexAction )
        con:act:_ -> ( tToLower con, tToLower act )
  in  rep { routeA = rep' }
    
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
renderRep :: Report -> ResourceT IO Response
renderRep rep = 
  let subRepToText (key,subReport) = 
        key := String (repToText $ applyActToSubRep a subRep)
        where (subRep,a) = confirmAct subReport 

      rep' = case subReports rep of 
        [] -> rep
        subReps -> rep  { subReports = []
                        , viewBson = 
                          (viewBson rep) ++ (map subRepToText subReps)
                        }

  in  return $ ResponseBuilder (status rep') (getResHeaders rep') $ 
        
      -- SOFTEN CODE HERE: there are other types of ResponseBuilders
      fromText $ 
      case viewTemplate rep of
        Nothing -> repToText rep'
        Just route -> repToText rep' -- rendered outer view
          { viewTemplate = Nothing
          , routeV = route
          , viewBson =              -- rendered inner view
              ( Hell.Lib.keyOfTemplatedView := String (repToText rep') )
            : ( Hell.Lib.keyOfMetaView := String (meta rep') )
            : viewBson rep' 
              -- TODO: soften these arguments.
          }

-- ***************************************************************************
-- | UNDER VOLATILE CONSTRUCTION
--
getResHeaders :: Report -> [Header]
getResHeaders rep = concat 
  [ resHeaders rep
  , [ ( "Set-Cookie", cookieToBS Hell.Lib.defaultCookie 
        { cookieName = Hell.Lib.sessionCookieName
        , cookieValue = B64.encode $ 
          encdoc 
          [ "name":= String "john"
          , "age":= Int32 23
          , "child":= Doc ["name":= String "jim"]
          ] 
        } 
      ) 
    ]
  ]
-- ***************************************************************************

-- | Takes Report from a Controller, returns a Text.
repToText :: Report -> Text
repToText r = fromMaybe
  (fromJust $ getView Hell.Lib.noSuchViewRoute)
  (getView (routeV r))
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
