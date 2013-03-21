{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude as P
import Database.MongoDB as M
import Data.Text.Read as T
import Data.List as L
import Data.Typeable
import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Random

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Jerng as J

--------------------------------------------------------------------------------
mongoDBMRto1 a  = let M.Array [M.Doc b] = M.valueAt "results" a 
                  in J.mongoDBPrint $ M.valueAt "_id" b 
--------------------------------------------------------------------------------
main = do
  args <- getArgs 
  let collection = T.pack $ head args
  pipe <- J.mongoDBGetPipe "127.0.0.1"
  accessed <- J.mongoDBAccessMaster pipe "wn" $ do 
    M.auth "hs" "hspassword" 
------
--    count <- M.count (M.select [] collection)
--    skipCount <- liftIO $ randomRIO (0,count-1)
--    M.rest =<< M.find (M.select [] collection) {limit=1,skip=(fromIntegral skipCount)}
------
--    runCommand  [ "mapreduce"=:collection,
--                  "scope"=:["counter"=:1,"getNumber"=:getNumber],
--                  "map"=:"function(){\
--                    \counter==getNumber ? emit(this,null) : null;\
--                    \counter++; }",
--                  "reduce"=:"function(key,values){return v}",
--                  "out"=:["inline"=:1]
--                ]
------
--    allCollections
------
--    M.rest =<< M.find (M.select [] "lex_files") {limit=3}
------
--    M.count (M.select [] collection)
------
--    insertMany "" verbSymbols
------
  M.close pipe
  J.debug accessed
  --T.putStrLn $ T.concat ["\ndb.",collection,", random document:"]
  --either J.debug mongoDBMRto1 accessed 
  --either J.debug J.mongoDBPrint accessed

  

{-----------------------------------------------------------------------------

db: wn, wn_v2
collections: 

    data_adj
    data_adv
    data_noun
    data_verb
      
    exc_adj
    exc_adv
    exc_noun
    exc_verb

    index_adj
    index_adv
    index_noun
    index_verb

    frames_verb'
    sentidx_verb' 
    sents_verb'

    index_sense'
    cntlist_rev'
    lexnames' 

-----------------------------------------------------------
    index_sense
      sense_key     - points to (see cntlist_rev)
      synset_offset - points to address in data_POS
      sense_number  - redundant (see cntlist_rev)
      tag_cnt       - redundant (see cntlist_rev) 
    
-----------------------------------------------------------
    frames_verb
      frame_number -> generic_sentence_frames
    sentidx_verb
      sense_key -> sentence_template_number (comma separated; see sents_verb)

    sents_verb 
      sentence_template_number -> sentence_template

-----------------------------------------------------------
    cntlist_rev
      tag_cnt       - decimal, number of tags in semantic concordances 
      sense_key     - (see below) 
      sense_number  - decimal, ordering, popularity of a sense

-----------------------------------------------------------
    sense_key (in: cntlist_rev, index_sense)

        lemma 
      % synset_type - 1:n, 2:v, 3:aj, 4:av, 5:aj satallite
      : lex_filenum - (see lexnames) 
      : lex_id      - decimal, UID for a sense, in a lex_file
      : head_word   - lemma, of the first word, 
                      of an adjective satellite's head synset.
      : head_id     - decimal, points to lex_id of the head_word
-------------------------------------------------------------------------------}

nounSymbols = 
  [
    ["!"=:"Antonym"],
    ["@"=:"Hypernym"],
    ["@i"=:"Instance Hypernym"],
    ["~"=:"Hyponym"],
    ["~i"=:"Instance Hyponym"],
    ["#m"=:"Member holonym"],
    ["#s"=:"Substance holonym"],
    ["#p"=:"Part holonym"],
    ["%m"=:"Member meronym"],
    ["%s"=:"Substance meronym"],
    ["%p"=:"Part meronym"],
    ["="=:"Attribute"],
    ["+"=:"Derivationally related form"],
    [";c"=:"Domain of synset - TOPIC"],
    ["-c"=:"Member of this domain - TOPIC"],
    [";r"=:"Domain of synset - REGION"],
    ["-r"=:"Member of this domain - REGION"],
    [";u"=:"Domain of synset - USAGE"],
    ["-u"=:"Member of this domain - USAGE"]
  ]

verbSymbols =
  [
    ["!"=: "Antonym"],
    ["@"=: "Hypernym"],
    ["~"=: "Hyponym"],
    ["*"=: "Entailment"],
    [">"=: "Cause"],
    ["^"=: "Also see"],
    ["REMOVETHIS$"=: "Verb Group"],
    ["+"=: "Derivationally related form"],
    [";c"=: "Domain of synset - TOPIC"],
    [";r"=: "Domain of synset - REGION"],
    [";u"=: "Domain of synset - USAGE"]
  ]

adjSymbols =
  [
    ["!"=:"Antonym"],
    ["&"=:"Similar to"],
    ["<"=:"Participle of verb"],
    ["\\"=:"Pertainym (pertains to noun)"],
    ["="=:"Attribute"],
    ["^"=:"Also see"],
    [";c"=:"Domain of synset - TOPIC"],
    [";r"=:"Domain of synset - REGION"],
    [";u"=:"Domain of synset - USAGE"]
  ]

advSymbols =
  [
    ["!"=:"Antonym"], 
    ["\\"=:"Derived from adjective"], 
    [";c"=:"Domain of synset - TOPIC"], 
    [";r"=:"Domain of synset - REGION"], 
    [";u"=:"Domain of synset - USAGE"] 
  ]

pointerReflexivity =
  [
    ["Antonym" =: "Antonym"],
    ["Hyponym" =: "Hypernym"],
    ["Hypernym" =: "Hyponym"],
    ["Instance Hyponym" =: "Instance Hypernym"],
    ["Instance Hypernym" =: "Instance Hyponym"],
    ["Holonym" =: "Meronym"],
    ["Meronym" =: "Holonym"],
    ["Similar to" =: "Similar to"],
    ["Attribute" =: "Attribute"],
    ["Verb Group" =: "Verb Group"],
    ["Derivationally Related" =: "Derivationally Related"],
    ["Domain of synset" =: "Member of Doman"]
  ]
