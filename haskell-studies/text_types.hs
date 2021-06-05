{- | CHECK THIS:

WITH -XOverloadedStrings, a multi-line text literal is escaped:

"first line terminates at the backslash;\
    this row of text is completely digested;\
 whitespace is not a problem."

WITHOUT -XOverloadedStrings, a multi-line text literal is escaped:

"first line terminates at the backslash;\
\    this row of text is completely digested;\
\ whitespace is not a problem."

BOTH are equivalent to:

"first line terminates at the backslash;    this row of text is completely digested; whitespace is not a problem."

-}

{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString as B
import Data.ByteString.Lazy as LB
  -- (ByteString) can be used for processing ASCII only as arrays
 
import Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 as LB
  -- These modules provide functions like those for manipulating [Char]s.
  -- They also contain instances of the (IsString) type class, 
  --  enabling -XOverloadedStrings to work with (ByteString)s.

import Data.ByteString.Search as B
import Data.ByteString.Lazy.Search as B
  -- These modules provide "search / split / replace" functions for (ByteString)s

import Data.ByteString.UTF8 as B
import Data.ByteString.Lazy.UTF8 as LB
  -- These modules have functions to convert between (String) and (ByteString)

import Data.Text as T
import Data.Text.Lazy as LT
  -- (Text) is used for processing Unicode as arrays

import Data.Text.Encoding as T
import Data.Text.Lazy.Encoding as LT
  -- These modules have functions to convert between (Text) and (ByteString)

