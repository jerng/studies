{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Jerng (  mongoDBGetPipe,
                mongoDBAccessMaster,
                mongoDBPrint,
                listSlice,
                listChunk,
                debug
             ) where

import Prelude as P
import qualified Database.MongoDB as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- boilerplate for M.access calls and pretty printing
mongoDBGetPipe hostname = M.runIOE $! M.connect $! M.host hostname

mongoDBAccessMaster pipe db actions = M.access pipe M.master db actions

mongoDBPrint accessed =
  T.putStr $! T.concat [T.replicate 80 "-","\n",mongoDBAccessedToText accessed]

-- takes output from M.access and turns it into type T.Text
class MongoDBAccessed a where 
  mongoDBAccessedToText :: a -> T.Text
instance MongoDBAccessed Int where
  mongoDBAccessedToText b = 
    T.pack $! show b ++ "\n\n(ToText Int)\n\n"
instance MongoDBAccessed M.Value where
  mongoDBAccessedToText b = 
    T.concat [showVal 0 b, "\n\n"]
instance MongoDBAccessed M.Document where
  mongoDBAccessedToText b = 
    T.concat [showDoc 0 b, "(ToText ([] Field))\n\n"]
instance MongoDBAccessed ([] M.Document) where
  mongoDBAccessedToText b = 
    T.concat [showBson b, "\n(ToText ([] Document)\n\n"]
instance MongoDBAccessed ([] M.Value) where
  mongoDBAccessedToText b = 
    T.concat [showArr 0 b, "\n(ToText ([] Values)\n\n"]
instance MongoDBAccessed ([] M.Collection) where
  mongoDBAccessedToText b = 
    T.concat [showCols b, "\n(ToText ([] Collection)\n\n"]

--------------------------------------------------------------------------------
-- LIBRARY : PRETTY PRINT FOR Data.Bson's types
-- (The following code remains inconsistent, and much improvable.)
-- ind : indentation levels 
-- doc : documents
-- arr : arrays
-- fld : fields
-- val : values
-- col : collection
showBson :: [M.Document] -> T.Text
showBson docList = T.intercalate "\n" (P.map (showDoc 0) docList)

showInd :: Int -> T.Text
showInd ind = T.replicate ind "\t"

showDoc :: Int -> M.Document -> T.Text
showDoc ind doc = T.concat $! "[\n"
  : T.intercalate ",\n" (P.map (showFld (ind+1)) doc)
  : "\n"
  : showInd ind
  : "]\n"
  : ( showInd ind )
  : []  

showArr :: Int -> [M.Value] -> T.Text
showArr ind arr = T.concat $! "[\n"
  : T.intercalate ",\n"
    (P.map (\v -> T.concat [ showInd (ind+1), showVal (ind+1) v ]) arr)
  : "\n"
  : showInd ind
  : "]\n"
  : showInd ind 
  : []

showFld :: Int -> M.Field -> T.Text
showFld ind fld@((M.:=) {M.label=l, M.value=v }) = T.concat $! showInd ind
  : T.pack(show l)
  : " =: "
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

showCols :: [M.Collection] -> T.Text
showCols cols = T.concat $! "[\n\t"
  : T.intercalate ",\n\t" cols
  : "\n]"
  : []

--------------------------------------------------------------------------------
-- from http://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
listSlice from to xs = take (to - from + 1) (drop from xs)

-- from http://hackage.haskell.org/packages/archive/split/0.1.1/doc/html/src/Data-List-Split-Internals.html#splitEvery
listChunk :: Int -> [e] -> [[e]]
listChunk n = takeWhile (not.null) . map (take n) . iterate (drop n)

--------------------------------------------------------------------------------
-- useful in (either debug somethingElse a) scenarios
debug a = T.putStrLn.T.pack $ show a
