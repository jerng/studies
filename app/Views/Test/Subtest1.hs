{-# LANGUAGE OverloadedStrings #-}
module Views.Test.Subtest1 where
import qualified Data.Text as T
import Hell.Lib
--import SomeResourceName2

text1 :: Reaction -> Text
text1 _ = "\
\\
\"

main :: Reaction -> Text
main reaction = T.concat [text1 reaction]