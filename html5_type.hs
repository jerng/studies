{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

type Html5'Document = [ Html5'Element ]

data Html5'Element = E 
  T.Text 
  [(T.Text,T.Text)]
  ( Maybe [ Html5'ElementContent ] )
    -- Nothing indicates a `void` element.

data Html5'ElementContent =
    Ct T.Text 
  | Ce Html5'Element
  | Cc Html5'Comment

data Html5'Comment = C T.Text

instance Show Html5'Comment where
  show (C t) = T.unpack $ T.concat ["<!--",t,"-->"]

instance Show Html5'Element where
  show (E t as mECs) = T.unpack $ T.concat
    [ "<"
    , t
    , T.concat $ map (\(a,v)->T.concat [" ",a,"=\"",v,"\""]) as
      -- note the rule 'attr' and 'attr=""' are equivalent
    , ">"
    , maybe "" (\x->T.pack $ (concatMap show x) ++ "</" ++ T.unpack t ++ ">") mECs
    ]

instance Show Html5'ElementContent where
  show a = case a of 
    Ct t -> T.unpack t
    Ce e -> show e
    Cc c -> show c

main = return $
  show body 

body :: Html5'Element
body = E "body" [] $ Just 
  [ Ce div'
  , Ct "Text in Body"
  , Cc comment
  ]

div' :: Html5'Element
div' = E "div" [("id","john"),("class","green")] $ Just [Ct "Text in div.", Cc comment]

comment :: Html5'Comment
comment = C "some comment"
