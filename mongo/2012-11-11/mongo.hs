{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude as P
import Database.MongoDB as M
import Data.Text.Read as T
import Data.List as L
import Control.Monad
import Control.Monad.Trans

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Jerng as J

--------------------------------------------------------------------------------
test a = mapM_ J.debug a
--------------------------------------------------------------------------------
main = do
  pipe <- J.mongoDBGetPipe "127.0.0.1"
  accessed <- J.mongoDBAccessMaster pipe "wn" $ do 
    M.auth "hs" "hspassword" 
------
--    runCommand  [   "mapreduce"=:"lex_files",
--                    "map"=:"function() { emit( this.name, null ) }",
--                    "reduce"=:"function(key,values){return v}",
--                    "out"=:["inline"=:1]
--                  ]
--    allCollections
    M.rest =<< M.find (M.select [] "lex_files")
--    M.count (M.select [] "lex_files")
------
  M.close pipe
  either J.debug J.mongoDBPrint accessed

  

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
