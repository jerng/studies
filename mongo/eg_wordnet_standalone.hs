{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude as P
import qualified Database.MongoDB as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

main = accessM >>= applyFormatting >>= toStdOut

{- SPECIFIC : MONGODB ACTION TO PERFORM
accessM :: IO (Either M.Failure Int)
otherActions :: M.Action IO Int 
otherActions = M.count $ M.select [] "data_adj"
--}
--{- SPECIFIC : MONGODB ACTION TO PERFORM
accessM :: IO (Either M.Failure [M.Document])
otherActions ::  M.Action IO [M.Document]
otherActions = do
  cursor <- M.find (M.select [] "data_adv") {M.limit=1,M.skip=1}
  M.rest cursor
--}

{-

data_adj
data_adv
data_noun
data_verb
  
exc_adj
exc_adv
exc_noun
exc_verb

index_adj
index_adv
index_noun
index_sense
index_verb

cntlist_rev
lexnames

frames_verb
sentidx_verb
sents_verb

-}

-- SPECIFIC : MONGODB CONNECTION & AUTHENTICATION CONFIGURATION
hostname = "127.0.0.1"
user = "hs"
password = "hspassword"
db = "wn"

-- GENERIC : CODE FOR CALLING TO MONGODB, AND PRETTY-PRINTING THE OUTPUT

-- accessM :: defined below; needs to be overloaded
accessM = do
  pipe <- M.runIOE $ M.connect $ M.host hostname 
  output <- M.access pipe M.master db (M.auth user password >> otherActions)
  M.close pipe
  return output

-- A giant, bulky, interface definition. Geez - just for overloading!
class Mongo a where
  applyFormatting :: Monad m => Either M.Failure a -> m T.Text 
instance Mongo M.Failure where
  applyFormatting (Left failure) = 
    return $ T.concat ["value of type M.Failure: \n\n", T.pack $ show failure]
instance Mongo Int where
  applyFormatting (Right int) = 
    return $ T.concat ["value of type Int: \n\n", T.pack $ show int]
instance Mongo ([] M.Document) where
  applyFormatting (Right docList) = 
    return $ T.concat [ "value of type [Document]:\n\n", showDocs docList ] 

toStdOut :: T.Text -> IO ()
toStdOut formatted = do
  T.putStrLn $ T.replicate 80 "-"
  P.mapM_ T.putStr ["\nmain -> ",formatted,"\n\n"]

-- LIBRARY : PRETTY PRINT FOR Data.Bson
-- (The following code is almost pretty, but not thought to be fast.)
-- ind : indentation levels 
-- doc : documents
-- arr : arrays
-- fld : fields
-- val : values
showDocs :: [M.Document] -> T.Text
showDocs docList = T.intercalate "\n" (P.map (showDoc 0) docList)

showInd :: Int -> T.Text
showInd ind = T.replicate ind "\t"

showDoc :: Int -> M.Document -> T.Text
showDoc ind doc = T.concat $ "[\n"
  : T.intercalate ",\n" (P.map (showFld (ind+1)) doc)
  : "\n"
  : showInd ind
  : "]\n"
  : ( showInd ind )
  : []  

showArr :: Int -> [M.Value] -> T.Text
showArr ind arr = T.concat $ "[\n"
  : T.intercalate ",\n"
    (P.map (\v -> T.concat [ showInd (ind+1), showVal (ind+1) v ]) arr)
  : "\n"
  : showInd ind
  : "]\n"
  : showInd ind 
  : []

showFld :: Int -> M.Field -> T.Text
showFld ind fld@((M.:=) {M.label=l, M.value=v }) = T.concat $ showInd ind
  : T.pack(show l)
  : " := "
  : showVal ind v
  : []

showVal :: Int -> M.Value -> T.Text
showVal ind val = case val of 
  M.Float v     -> T.concat [T.pack(show v)," (Float Double)"]
  M.String v    -> T.concat [T.pack(show v)," (String Text)"]
  M.Doc v       -> T.concat [showDoc ind v,"(Doc Document)"]
  M.Array v     -> T.concat [showArr ind v,"(Array [Value])"]
  M.Bin v       -> T.concat [T.pack(show v)," (Bin Binary)"]
  M.Fun v       -> T.concat [T.pack(show v)," (Fun Function)"]
  M.Uuid v      -> T.concat [T.pack(show v)," (Uuid UUID)"]
  M.Md5 v       -> T.concat [T.pack(show v)," (Md5 MD5)"]
  M.UserDef v   -> T.concat [T.pack(show v)," (UserDef UserDefined)"]
  M.ObjId v     -> T.concat [T.pack(show v)," (ObjId ObjectId)"]
  M.UTC v       -> T.concat [T.pack(show v)," (UTC UTCTime)"]
  M.Null        ->                          "Null (Null)"
  M.RegEx v     -> T.concat [T.pack(show v)," (RegEx Regex)"]
  M.JavaScr v   -> T.concat [T.pack(show v)," (JavaScr Javascript)"]
  M.Sym v       -> T.concat [T.pack(show v)," (Sym Symbol)"]
  M.Int32 v     -> T.concat [T.pack(show v)," (Int32 Int32)"]
  M.Int64 v     -> T.concat [T.pack(show v)," (Int64 Int64)"]
  M.Stamp v     -> T.concat [T.pack(show v)," (Stamp MongoStamp)"]
  M.MinMax v    -> T.concat [T.pack(show v)," (MinMax MinMaxKey)"]

