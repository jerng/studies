{-# LANGUAGE OverloadedStrings #-} 

import Data.Binary (Binary)
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet)
import Data.Bson 
import Data.Bson.Binary (putDocument,getDocument)
import qualified Data.ByteString as BS (ByteString, concat) 
import Data.ByteString.Lazy (fromChunks, toChunks)

main :: IO Document
main = return $ decdoc  $
    encdoc 
    [ "name":= String "john"
    , "age":= Int32 23
    , "child":= Doc ["name":= String "jim"]
    ] :: Document

encdoc :: Document -> BS.ByteString
encdoc doc = BS.concat $ toChunks $ runPut $ putDocument doc 

decdoc :: BS.ByteString -> Document
decdoc bin = runGet getDocument $ fromChunks [ bin ]
