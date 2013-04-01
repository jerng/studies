-- | Attempt to form nestable, binary maps.
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


-- import Codec.Compression.GZip
  -- test compression with (compress), (decompress)
import Data.Binary (Binary,get,put,Get)
import qualified Data.Binary as Bin
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
--import Data.List
--import Data.List.Utils
--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word (Word8)


main :: IO [N2pair]
--main :: IO ByteString
main = return $ decode $ encode structure4


structure4 :: [N2pair]
structure4 =  [ "boy"...("george"::Text)
              , "girl"...("green"::Text)
              , "count"...(2::Int)
              , "wispy"...(2.34::Float)
              , "rabbit hole" ... ["one"...("potato"::Text), "two"...("potato"::Text)]
              ]

structure3 =
  [ N2int "age" 41 
  , N2text "name" "John"
  , N2branch "child"  [ N2int "age" 3
                      , N2text "name" "Jimmy"
                      , N2float "mph" 0.03
                      ]
  ] 

instance Binary N2pair where
  put (N2branch t n2pairList) 
    = do  put (0::Word8)
          put t 
          put n2pairList
  put (N2int t i)             
    = do  put (1::Word8)
          put t
          put i
  put (N2float t f)           
    = do  put (2::Word8)
          put t
          put f
  put (N2text t t')           
    = do  put (3::Word8)
          put t
          put t'
  get 
    = do  a <- get :: Get Word8
          case a of
            0 -> do t <- get
                    n2pairList <- get
                    return (N2branch t n2pairList)
            1 -> do t <- get
                    i <- get
                    return (N2int t i)
            2 -> do t <- get
                    f <- get
                    return (N2float t f)
            3 -> do t <- get
                    t' <- get
                    return (N2text t t')

data N2pair = N2branch Text [N2pair]
            | N2int Text Int 
            | N2float Text Float 
            | N2text Text Text
            deriving (Show)

class (Binary a) => Encodable a where
  (...) :: Text -> a -> N2pair

instance Encodable [N2pair] where
  (...) t l = N2branch t l
  
instance Encodable Int where
  (...) t i = N2int t i

instance Encodable Float where
  (...) t f = N2float t f

instance Encodable Text where
  (...) t t' = N2text t t'

instance Binary Text where
  put text = do put (encodeUtf8 text)
  get = do  text <- get
            return (decodeUtf8 text)

encode :: Binary a => a -> ByteString
encode a = BS.concat $ LBS.toChunks $ Bin.encode a

decode :: Binary a => ByteString -> a 
decode bin = Bin.decode $ LBS.fromChunks [ bin ]

