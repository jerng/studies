{-# LANGUAGE OverloadedStrings #-}
 
module Hell.Parse.Forms
  ( file'ToMaybeValue
  , queryItem'ToMaybeValue
  , mergeRecursivel1
  ) where

import Data.Bson as Bson (Document, Field(..), Value (..), Label, Binary(..), look)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.UTF8 as BS.UTF8 
import Data.List (nub)
import qualified Data.Text.Encoding as T
import Hell.Attributed
import Hell.Types
import Network.Wai.Parse (File,FileInfo(..))


-- Labels, if any are found, are returned with the inner-most label first.
-- Also returns Type.
inputName'ToMaybeKeys'AndType 
  :: BS.ByteString -> Maybe ([BS.ByteString],BS.ByteString)
inputName'ToMaybeKeys'AndType inputName = 
  let tuple = case BS.split ':' inputName of
              ["data",ls,t]   -> ( reverse $ BS.split '.' ls, t )
              otherwise       -> ( [], "" )
  in  case tuple of
      ( []    , _ ) -> Nothing -- Debug "no collection/sub-keys found"
      ( l:[]  , _ ) -> Nothing -- Debug "collection, but no sub-keys, found"
      x             -> Just x

file'ToMaybeValue :: File LBS.ByteString -> Maybe Value  
file'ToMaybeValue (inputName, fInfo) = 
  let fName         = fileName fInfo
      fContent      = fileContent fInfo
  in  if    and [fName=="\"\"", fContent==""]
      then  Nothing
      else  case inputName'ToMaybeKeys'AndType inputName of
            Nothing     -> Nothing
            Just (ls,_) -> Just $ -- (_) ought to be "Document"; assume it is.
              ( let foldSeed      = Array 
                      [ Bin $ Binary fName 
                      , Bin $ Binary $ fileContentType fInfo
                      , Bin $ Binary $ BS.concat $ LBS.toChunks fContent
                      ]
                in  foldl nestValuesl foldSeed ls
              )

-- Not sure if this can be made simpler.
queryItem'ToMaybeValue :: (BS.ByteString, Maybe BS.ByteString) -> Maybe Value 
queryItem'ToMaybeValue (inputName, maybeUnparsedValue) = 
  case inputName'ToMaybeKeys'AndType inputName of
  Nothing     -> Nothing
  Just (ls,t) -> Just $ 
    ( let foldSeed = maybe Null ( textToBsonValue t ) maybeUnparsedValue
                    -- Null would imply a "key&" situation (no =)
      in  foldl nestValuesl foldSeed ls
    )

nestValuesl :: Value -> BS.ByteString -> Value
nestValuesl v l = Doc [ T.decodeUtf8 l := v ]

-- Takes the output of a document processed with (nestValuesl).
-- Fugly: optimise later.
mergeRecursivel1 :: [Value] -> Document
mergeRecursivel1 docs = foldl f [] docs

  where
  f :: Document -> Value -> Document
  f d dValue' = map f' labels

    where
    Doc d' = dValue'
    labels = nub $ getLabels d ++ getLabels d'
    
    f' :: Label -> Field
    f' l = case (e,e') of
      (Just (Doc val), Just (Doc val')) -> l := Doc ( f val $ Doc val' )
        -- if they are both Docs, then recurse
      (Just value, Just value')         -> l := value' 
        -- take the second / RHS
      (Nothing, Just value')            -> l := value'
      (Just value, Nothing)             -> l := value

      where 
      e = Bson.look l d :: Maybe Value
      e'= Bson.look l d':: Maybe Value 
      -- this is so unintuitive, it's disgusting

getLabels :: Document -> [Label]
getLabels doc = map (\(l := _)->l) doc

-- | HTML form, input element, where input[name] is of the form "data:ks:t"
-- and input[value] as submitted to the server is v.
-- This documentation can be improved.
textToBsonValue :: BS.ByteString -> BS.ByteString -> Value
textToBsonValue t v = 
  let f x y = (maybe Null y) . readMaybe . BS.UTF8.toString  $  x
  in  case t of
      "Text"    ->  String . T.decodeUtf8  $  v
      otherwise ->  
        case t of
          "Int32"   -> f v Int32
          "Double"  -> f v Float 
          "Bool"    -> f v Bool
--            "Document"
--            "[Value]"
--            "Function"
--            "UUID"
--            "MD5"
--            "UserDefined"
--            "ObjectId"
--            "Bool"
--            "UTCTime"
--            " Null"
--            "Regex"
--            "Javascript"
--            "Symbol"
--            "Int64"
--            "MongoStamp"
--            "MinMaxKey"
          otherwise -> Null
-- add Debug here, later
