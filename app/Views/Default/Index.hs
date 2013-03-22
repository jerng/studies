{-# LANGUAGE OverloadedStrings #-}
module Views.Default.Index where
import qualified Data.Text as T
import Hell.Lib
--import SomeResourceName2

text1 :: Reaction -> Text
text1 _ = "<!DOCTYPE html>\
\<html>\
\  <head>\
\    <title>frameworkRnD</title>\
\  </head>\
\  <body>\
\    <h1>a simple view: </h1> \
\    Following this is an attempt to interpolate some Haskell expressions<br/>\
\    1 +1 : "


text2 :: Reaction -> Text
text2 (Reaction status route textMap) = T.pack $ show $ (1+1)

text3 :: Reaction -> Text
text3 _ = "<br/>\
\    a variable called (someVar) : "


text4 :: Reaction -> Text
text4 (Reaction status route textMap) = fromJust $ lookup "someVar" textMap


text5 :: Reaction -> Text
text5 _ = "<br/>\
\    <br/>\
\    The objective here is to run all of this inside a (do{}) statement, within\
\    a (Controller.action), so that the scope is confined to that.<br/>\
\    <br/>\
\    I had earlier thought of using the architecture where the (Controller.action)\
\    returns parameters and control to (Server.main) to have rendering done\
\    elsewhere, at a higher level, but the passing of variables seems to involve\
\    much boilerplate. I'm still thinking about this. And about how much time \
\    this is consuming to no definite end, subtracting from  my general scheme of \
\    studies.\
\  </body>\
\</html>\"\
\"

main :: Reaction -> Text
main reaction = T.concat [text1 reaction,text2 reaction,text3 reaction,text4 reaction,text5 reaction]