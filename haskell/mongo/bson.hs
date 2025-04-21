{-# LANGUAGE OverloadedStrings #-}

import Data.Bson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T


main = return $ 
  lexJSQ 
  --".something.something[0][234].something"
  --"something.something[0][234].something" -- missing [ or .
  --"..something.something[0][234].something" -- empty string 
  --".something.something.[0][234].something" -- empty string 

  --".something.something[[0][234].something" -- extra [
  --".something.something][0][234].something" -- extra ]
  --".something.something[][234].something" -- missing integer
  ".something.something[0a][234].something" -- extra non-integer 
  --"].something.something[0][234].something" -- extra closer

  -- after all that, we'll still have problems with special characters.

type JavaScriptSyntaxAddress = Text 
data BsonKey  = DocKey Text  -- document keys
              | ArrayKey Int   -- array keys
              deriving (Show)

{- TESTS:

legitimate addresses:
  
  .a                    maps to Just $ String "z"
  .c.dsd.df             maps to Just $ Int32 74
  .something[0].x       maps to Just $ String "sdfsdf"
  .something[4][3]      maps to Just $ Float 234.22

lexJSQ :: JavaScriptSyntaxAddress -> [BsonKey]

-}

lexJSQ :: Text -> [BsonKey]
lexJSQ text = case text of 
  ""  -> []
  _   -> reverse $ notInToken text []

notInToken :: Text -> [BsonKey] -> [BsonKey]
notInToken text acc = case text of
  ""        -> acc
  otherwise ->  case T.splitAt 1 text of 
    (".",rem) -> inTextToken rem acc
    ("[",rem) -> inIntToken rem acc 
    ("]",_) -> error "notInToken: unexpected ']'"
    otherwise -> error $
      "notInToken: expected a '.', '[', or ']' at the beginning of \"" 
      ++ (T.unpack text)
      ++ "\""


inTextToken :: Text -> [BsonKey] -> [BsonKey]
inTextToken text acc = case T.span (\x-> and [x/='.',x/='[']) text of
  (token,rem) -> notInToken rem $ (DocKey token):acc

inIntToken :: Text -> [BsonKey] -> [BsonKey]
inIntToken text acc = case T.span (/=']') text of
  (token,rem) -> expectSquareCloser rem $ 
    (ArrayKey (read $ T.unpack $ token :: Int)):acc

expectSquareCloser :: Text -> [BsonKey] -> [BsonKey]
expectSquareCloser text acc = case T.splitAt 1 text of
  ("]",rem) -> notInToken rem acc
  otherwise -> error "expectSquareCloser: did not find a closeing ']'"








--main :: IO (Maybe Value)
--main = return $ lookupBson (Doc doc)  ""

lookupBson :: Value -> JavaScriptSyntaxAddress -> Maybe Value
lookupBson value jsq = Nothing




















doc :: Document 
doc = ["a" := String "z", "b" := Int32 43, "c" := Doc subdoc, "something":=arrayarray]

subdoc :: Document
subdoc = ["ca" := Int32 45,"dsd":=Doc subsubdoc, "mko":=String "sdfs"]

subsubdoc :: Document
subsubdoc = ["df" := Int32 74]

array :: Value
array = Array [Int32 7,String "sss",Float 234.22]

arrayarray :: Value
arrayarray =  Array [ Doc ["x":=String"sdfsdf"]
              , Array [String"y"]
              , Array [Int32 345]
              , Array [Null]
              , array
              ]

-- modified from Data.List.lookup
lookupBsonValue :: Label -> Document -> Maybe Value 
lookupBsonValue _key [] = Nothing
lookupBsonValue  key (field:exhead)
    | key == (label field) =  Just (value field)
    | otherwise = lookupBsonValue key exhead

-- returns Nothing if: key is not found, or type signature does not match
--main :: IO (Maybe Int)
--main = return $ lookupBsonVal "b" doc

lookupBsonVal :: Val a => Label -> Document -> Maybe a
lookupBsonVal _key [] = Nothing
lookupBsonVal  key (field:exhead)
    | key == (label field) =  (cast' =<< Just (value field))
    | otherwise = lookupBsonVal key exhead

-- returns IO True
--main = do 
--  x <- (look "b" doc) 
--  return $ x == Int32 43

-- returns IO Value
--main :: IO Value
--main = look "b" doc

-- returns Just (Int32 43)
--main :: IO Value
--main = look "b" doc

-- returns IO (Just 45) 
--main :: IO ( Maybe Int )
--main = return $ doc !? "c.ca"

-- returns Nothing (failure due to missing key)
--main :: IO ( Maybe Document )
--main = return $ doc !? "d"

-- returns Nothing (failure due to mismatched type signature)
--main :: IO ( Maybe Int )
--main =  return $ doc !? "c" 

-- returns Just subdoc 
--main :: IO ( Maybe Document )
--main =  return $ doc !? "c" 

-- returns doc
--main :: IO Document
--main =  return doc

-- both work
--main :: IO Int
--main = return $ typed $ fromJust $ lookupBsonValue "b" doc
--main = return $ fromJust $ cast' =<< (lookupBsonValue "b" doc)

-- works
--main :: IO (Maybe Value)
--main = return $ lookupBsonValue "b" doc

