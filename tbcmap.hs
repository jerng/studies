-- | Attempt to form nestable, binary maps.
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- import Codec.Compression.GZip
  -- test compression with (compress), (decompress)
import Data.Binary (Binary,get,put)
import qualified Data.Binary as Bin
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.List.Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding

type Tbc = (Text,(ByteString,Char))

instance Binary Text where
  put text = do put (encodeUtf8 text)
  get = do  text <- get
            return (decodeUtf8 text)


main = return $ Bin.encode structure1

structure1 :: [Tbc]
structure1 = 
  [ pack "user" ( 234234 :: Int ) 
  , pack "name"( "John" :: Text ) 
  , pack "years" ( 34.4567 :: Float)
  , pack "child" structure2
  ]

structure2 :: [Tbc]
structure2 =
  [ pack "user" ( 23422344 :: Int ) 
  , pack "name"( "Jim" :: Text ) 
  , pack "years" ( 4.47 :: Float)
  ]


class Encodable a where 
  pack :: Text -> a -> Tbc

instance Encodable Int where 
  pack text a = (text,(BS.concat $ LBS.toChunks $ Bin.encode a,'i'))

instance Encodable Float where 
  pack text a = (text,(BS.concat $ LBS.toChunks $ Bin.encode a,'f'))

instance Encodable Text where 
  pack text a = (text,(BS.concat $ LBS.toChunks $ Bin.encode a,'t'))

instance Encodable [Tbc] where 
  pack text a = (text,(BS.concat $ LBS.toChunks $ Bin.encode a,'z'))

