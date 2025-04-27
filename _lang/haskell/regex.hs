import Data.Array.IArray
import Data.ByteString.Char8 as B
import Prelude as P
import Text.Regex.PCRE

--import Text.Regex.TDFA
  -- Figure this out later.

main = return $ 

  -- all expressions returned by the functions below are (ByteString)s

  -- basic (=~)
  --{-
  ( (pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)")
    :: 

    --Bool 
      -- True, if there was a match

    --()
      -- guard, for =~~, refer to Text.Regex.Base.Context docs

    --ByteString
      -- text of the FIRST matched expression

    --(MatchOffset,MatchLength)
      -- of the FIRST matched expression

    --(ByteString, ByteString, ByteString)
      -- text before, of, and after the FIRST matched expression

    (ByteString, ByteString, ByteString, [ByteString])
      -- text before, of, and after the FIRST matched expression; 
      -- the text of all its matched subexpressions;
      -- intended to be the same return type as Text.Regex.matchRegexAll

    --MatchArray
      -- Array of (MatchOffset,MatchLength) 
      -- for the FIRST matched expression, and its matched subexpressions

    )

  --}  

  -- (=~) and the (MatchResult) type
  -- Results are returned for the FIRST matched expression, 
  -- and its matched subexpressions
  {-
  --mrBefore
    -- text before expression
  --mrMatch
    -- text of expression
  --mrAfter
    -- text after expression
  --mrSubList
    -- List of subexpressions
  --mrSubs
    -- Array of expression, and subexpressions
   (pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") :: (MatchResult ByteString) )
  --}

  -- (=~) versus (match)
  -- THIS:
  {-
            ( (pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") :: Bool )
  --}
  -- ...IS A CONVENIENCE function/opperator, FOR THIS:
  {-
            ( match     ( makeRegex ( pack "(b).*?(c)") :: Regex ) 
                        ( pack "abcdebxcfgfbycijk" )  
              :: Bool )
  --}

  -- AllSubmatches
  {-
  getAllSubmatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllSubmatches [] (MatchOffset,MatchLength))
  -- A List of: 
  --  (MatchOffset, MatchLength)s,
  --  for the FIRST matched expression, and its matched subexpressions -}

  -- AllTextSubMatches-
  {-
  getAllTextSubmatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextSubmatches (Array Int) ByteString)
  -- An Array of: 
  --  (expression, (MatchOffset, MatchLength))s,
  --  for the FIRST matched expression -}

  {-
  getAllTextSubmatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextSubmatches [] ByteString)
  -- A List of: 
  --  (expression, (MatchOffset, MatchLength))s,
  --  for the FIRST matched expression -}

  {-
  getAllTextSubmatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextSubmatches [] (ByteString, (MatchOffset, MatchLength)))
  -- A List of: 
  --  (expression, (MatchOffset, MatchLength))s,
  --  for the FIRST matched expression, and its matched subexpressions-}

  {-
  getAllTextSubmatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextSubmatches (Array Int) (ByteString, (MatchOffset, MatchLength)))
  -- An Array of: 
  --  (expression, (MatchOffset, MatchLength))s,
  --  for the FIRST matched expression, and its matched subexpressions-}

  -- AllMatches
  {-
  getAllMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllMatches (Array Int) MatchArray)
  -- An Array of: 
  --  (MatchArrays)s, i.e. Arrays of:
  --    (MatchOffset,MatchLength)s
  --    for ALL matched expressions, and their matched subexpressions -}

  {-
  getAllMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllMatches (Array Int) (MatchOffset, MatchLength))
  -- An Array of: 
  --  (MatchOffset, MatchLength)s
  --  for ALL matched expressions -}

  {-
  getAllMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllMatches [] (MatchOffset, MatchLength))
  -- A List of: 
  --  (MatchOffset, MatchLength)s,
  --  for ALL matched expressions -}

  -- AllTextMatches
  {-
  getAllTextMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextMatches (Array Int) (Array Int ByteString))
  --  An Array of: 
  --    Arrays of:
  --      expressions, 
  --      for ALL matched expressions, and their matched subexpressions -}

  {-
  getAllTextMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextMatches [] (Array Int ByteString))
  --  A List of: 
  --    Arrays of:
  --      expressions, 
  --      for ALL matched expressions, and their matched subexpressions -}

  {-
  getAllTextMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextMatches (Array Int) [ByteString])
  --  An Array of: 
  --    Lists of: 
  --      expressions, 
  --      for ALL matched expressions, and their matched subexpressions -}

  {-
  getAllTextMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextMatches (Array Int) ByteString)
  --  An Array of: 
  --    expressions, 
  --    for ALL matched expressions -}

  {-
  getAllTextMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "b.*?c") 
    :: AllTextMatches [] ByteString)
  --  A List of: 
  --    expressions, 
  --    for ALL matched expressions -}

  {-
  getAllTextMatches 
  ((pack "abcdebxcfgfbycijk") =~ (pack "(b).*?(c)") 
    :: AllTextMatches (Array Int) (MatchText ByteString))
  -- An Array of: 
  --  (MatchText)s, i.e. Arrays of:
  --    (expression, (MatchOffset, MatchLength)s 
  --    for ALL matched expressions, and their matched subexpressions -}









{-
  regexReplace
    (pack "(b).*?(c)")
    (pack "XXX")
    (pack "abcdebxcfgfbycijk") 

--  Following code from : 
--  https://mutelight.org/generating-a-permalink-slug-in-haskell
--{- | Replace using a regular expression. ByteString version.
regexReplace ::
    ByteString          -- ^ regular expression
    -> ByteString       -- ^ replacement text
    -> ByteString       -- ^ text to operate on
    -> ByteString
regexReplace regex replacement text = go text []
    where go str res =
              if B.null str
              then B.concat . P.reverse $ res
              else case (str Text.Regex.PCRE.=~~ regex) :: Maybe
                (ByteString,
                ByteString,
                ByteString) of
                     Nothing -> B.concat . P.reverse 
                                $ (str:res)
                     Just (bef, _ , aft) -> go aft (replacement:bef:res)
--}

-}










{-
getAllTextMatches ("foo" =~  "o" :: AllTextMatches [] String)
"xyz abc more text" =~ "(\\w+) \\w+" :: [[String]]
"before foodiebar after" =~ pat :: (String,String,String,[String])
-}














{-
{-# LANGUAGE OverloadedStrings #-} 
-- | Get all sub-matches
-- Replace all sub-matches
-- Overload for Text, ByteString, and lazy ByteString (common fast formats)
-- If not overload, at least provide easy conversion from String (via-a-vis 
-- Prelude).


import Text.Regex.PCRE.ByteString
import Data.ByteString.Char8
import Data.Array (Array)

-- Gets the before / match / after ByteStrings.
_regexec
  :: IO
       (Either
          WrapError
          (Maybe (ByteString, ByteString, ByteString, [ByteString])))
_regexec = do
  regex <- _regex
  regexec regex "abCdefgbChijklmbCno" 

-- Gets the first match offset and length.
_execute
  :: IO
       (Either
          WrapError (Maybe (Array Int (MatchOffset, MatchLength))))
_execute = do
  regex <- _regex
  execute regex "abCdefgbChijklmbCno"

_regex :: IO Regex
_regex = do
  --compiled <- compile compMultiline execBlank "b.(.*?)b."
  compiled <- compile compMultiline execBlank "bC"
    -- These compX and execY settings correspond to defaulst defined in
    -- Text.Regex.PCRE.Wrap
  case compiled of
    Right regex 
      -> return regex
-}
























{-  Notes

CONTINUE: 

          look in Text.Regex.PCRE.Wrap, 
            for the (data) declaration of (Regex),
            for the (function) declaration of (=~)

          look in Text.Regex.Base.RegexLike,
            for the (class function) declaration of (makeRegex) 

          look in Text.Regex.PCRE.ByteString,
            for the (type class instance/method) declarations of 
              RegexMaker
              RegexLike
              RegexContext

NOTES:

RegexMaker is a subclass of RegexOptions.
RegexContext is a subclass of RegexLike; RegexLike is a subclass of Extract.
"Extract allows for indexing operations on String or ByteString."

Type class declaration of (RegexLike):
  class Extract source 
    =>  RegexLike -- HERE
        regex 
        source 
    where

Type class declaration of (RegexContext):
  class RegexLike regex source 
    =>  RegexContext -- HERE
        regex 
        source 
        target 
    where

Type class declaration of (RegexOptions):
  class RegexOptions -- HERE
        regex 
        compOpt 
        execOpt
        |   regex->compOpt execOpt, 
            compOpt->regex execOpt, 
            execOpt->regex compOpt 
    where

Type class declaration of (RegexMaker):
  class RegexOptions regex compOpt execOpt 
    =>  RegexMaker -- HERE
        regex 
        compOpt 
        execOpt 
        source
        |   regex -> compOpt execOpt, 
            compOpt -> regex execOpt, 
            execOpt -> regex compOpt 
    where

-}

