{-# LANGUAGE OverloadedStrings #-}

import Data.Bson
import Data.Maybe
import Data.Text (Text)


main :: IO (Maybe Value)
main = return $ lookupBson (Doc doc)  ""

lookupBson :: Value -> JavaScriptSyntaxAddress -> Maybe Value
lookupBson value jsq = Nothing

type JavaScriptSyntaxAddress = Text 

{-

legitimate addresses:
  
  ['a']                 maps to Just $ String "z"
  .a                    maps to Just $ String "z"
  
  ['c']['dsd']['df]     maps to Just $ Int32 74
  .c.dsd.df             maps to Just $ Int32 74
  
  ['something'][0]['x'] maps to Just $ String "sdfsdf"
  .something[0].x       maps to Just $ String "sdfsdf"
  
  ['something'][4][3]   maps to Just $ Float 234.22
  .something[4][3]      maps to Just $ Float 234.22

-}

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

