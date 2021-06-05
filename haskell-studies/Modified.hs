{- | This module contains functions that have been taken from other libraries,
and minimally modified. I've tried to annotate the MODIFICATIONs. -}

{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE BangPatterns #-}
  -- for (toStrict)
{-# LANGUAGE TemplateHaskell #-}

module Modified where

import qualified Data.ByteString                as S 
import qualified Data.ByteString.Lazy           as LB 
import qualified Data.ByteString.Lazy.Internal  as LB
import qualified Data.ByteString.Internal       as S
import qualified Data.ByteString.Unsafe         as S
import qualified Foreign.C.Types                as C
import qualified Foreign.ForeignPtr             as FP
import qualified Foreign.Ptr                    as P

import Language.Haskell.TH.Syntax

-- | This function is from (bytestring-0.10.2.0), as it's unavailable in
-- (bytestring-0.9.2.1).
toStrict :: LB.ByteString -> S.ByteString
toStrict LB.Empty               = S.empty
toStrict (LB.Chunk c LB.Empty)  = c
toStrict cs0 = S.unsafeCreate totalLen $ \ptr -> go cs0 ptr
  where
    totalLen = LB.foldlChunks (\a c -> a + S.length c) 0 cs0
    go LB.Empty                        !_       = return ()
    go (LB.Chunk (S.PS fp off len) cs) !destptr =
      FP.withForeignPtr fp $ \p -> do
        let len' = C.CSize (fromIntegral len)
          -- MODIFICATION
        S.memcpy destptr (p `P.plusPtr` off) len'
        go cs (destptr `P.plusPtr` len)

-- | UNCHECKED; UNOPTIMISED 
-- Following code from : 
-- https://mutelight.org/generating-a-permalink-slug-in-haskell
--subRegex' ::
--    ByteString          -- ^ regular expression
--    -> ByteString       -- ^ replacement text
--    -> ByteString       -- ^ text to operate on
--    -> ByteString
--subRegex' regex replacement text = go text []
--    where go str res =
--              if B.null str
--              then B.concat . P.reverse $ res
--              else case (str Text.Regex.PCRE.=~~ regex) :: Maybe
--                (ByteString,
--                ByteString,
--                ByteString) of
--                     Nothing -> B.concat . P.reverse 
--                                $ (str:res)
--                     Just (bef, _ , aft) -> go aft (replacement:bef:res)

-- | Following code from : http://stackoverflow.com/questions/5480228/
-- how-to-get-the-current-module-name-in-haskell


moduleOf :: Language.Haskell.TH.Syntax.Name -> String
moduleOf = dropLastToken . show

dropLastToken :: String -> String
dropLastToken = reverse . tail . dropWhile (/= '.') . reverse
-------------------------------------------------------------------------------
