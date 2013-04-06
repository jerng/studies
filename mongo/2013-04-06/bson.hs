{-# LANGUAGE OverloadedStrings #-}

import Data.Bson
import Data.Maybe

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

doc :: Document 
doc = ["a" := String "z", "b" := Int32 43, "c" := Doc subdoc]

subdoc :: Document
subdoc = ["ca" := Int32 45]

-- both work
--main :: IO Int
--main = return $ typed $ fromJust $ lookupBsonValue "b" doc
--main = return $ fromJust $ cast' =<< (lookupBsonValue "b" doc)

-- works
--main :: IO (Maybe Value)
--main = return $ lookupBsonValue "b" doc

-- modified from Data.List.lookup
lookupBsonValue :: Label -> Document -> Maybe Value 
lookupBsonValue _key [] = Nothing
lookupBsonValue  key (field:exhead)
    | key == (label field) =  Just (value field)
    | otherwise = lookupBsonValue key exhead

-- returns Nothing if: key is not found, or type signature does not match
main :: IO (Maybe Int)
main = return $ lookupBsonVal "b" doc

lookupBsonVal :: Val a => Label -> Document -> Maybe a
lookupBsonVal _key [] = Nothing
lookupBsonVal  key (field:exhead)
    | key == (label field) =  (cast' =<< Just (value field))
    | otherwise = lookupBsonVal key exhead

