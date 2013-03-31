-- | Attempt to form nestable, binary maps.
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Npair where

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

type Npair = (Text,(ByteString,Char))

instance Binary Text where
  put text = do put (encodeUtf8 text)
  get = do  text <- get
            return (decodeUtf8 text)

class (Binary a ) => Encodable a where 
  pack :: Text -> a -> Npair
  (...) :: Text -> a -> Npair 
  unpack :: Text -> [Npair] ->  a

instance Encodable Int where 
  pack t a = (t,(encode a,'i'))
  (...) = pack
  unpack t nmap = 
    case lookup t nmap of
      Nothing -> error "unpack Nmap" 
      Just (bin,c) ->  (decode bin) 

instance Encodable Float where 
  pack t a = (t,(encode a,'f'))
  (...) = pack
  unpack t nmap = 
    case lookup t nmap of
      Nothing -> error "unpack Nmap"
      Just (bin,c) ->  (decode bin) 

instance Encodable Text where 
  pack t a = (t,(encode a,'t'))
  (...) = pack
  unpack t nmap = 
    case lookup t nmap of
      Nothing -> error "unpack Nmap"
      Just (bin,c) ->  (decode bin) 

instance Encodable [Npair] where 
  pack t a = (t,(encode a,'z'))
  (...) = pack
  unpack t nmap = 
    case lookup t nmap of
      Nothing -> error "unpack Nmap"
      Just (bin,c) ->  (decode bin) 

encode :: Binary a => a -> ByteString
encode a = BS.concat $ LBS.toChunks $ Bin.encode a

decode :: Binary a => ByteString -> a 
decode bin = Bin.decode $ LBS.fromChunks [ bin ]

-- | (Map.insert) offers a favourable syntax;
-- (Data.List.Utils) offers an existing semantic;
-- here we bridge these.
--insert :: Eq key => key -> elt -> [(key, elt)] -> [(key, elt)]
--insert key a map = addToAL map key a

--{- Example usage:

--main :: IO [Npair] 
--main = return $ structure1 

--main :: IO ByteString
--main = return $ encode structure1



main :: IO [N2pair]
main = return [ N2int "age" 41 
              , N2text "name" "John"
              , N2branch "child"  [ N2int "age" 3
                                  , N2text "name" "Jimmy"
                                  , N2float "mph" 0.03
                                  ]
              ] 

instance Binay N2pair where
  put = 
  get =

data N2pair = N2branch Text [N2pair]
            | N2int Text Int 
            | N2float Text Float 
            | N2text Text Text
            deriving (Show)


-- recursiveKeys = 
--main = return $ keysAL decoded1

decoded1 :: [Npair]
decoded1 = decode encoded1

encoded1 :: ByteString
encoded1 = encode structure1

structure1 :: [Npair]
structure1 = 
  [ "user" ... (234234 :: Int ) 
  , "name" ... ("John" :: Text ) 
  , "years" ... (34.4567 :: Float)
  , "child" ... structure2
  ]

structure2 :: [Npair]
structure2 =
  [ "user" ... (23422344 :: Int ) 
  , "name" ... ("Jim" :: Text ) 
  , "years" ... (4.47 :: Float)
  ]

--}
