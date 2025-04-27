main = return [x | x <- "hello", y <- [1..3]]

{- ***************************************************************************

LIST COMPREHENSIONS are syntactic sugar for do-notation over the List monad
http://stackoverflow.com/questions/8029046/removing-syntactic-sugar-list-comprehension-in-haskell

[x | x <- "hello", y <- [1..3]]

... results in
 
  (vii)
  "hhheeellllllooo"

  via the intermediate steps

  (i)
  do  x <- "hello"
      y <- [1..3]
      return x

  (ii)
  "hello" >>= \x->
  [1..3]  >>= \y->
  return x

  (iii)
  "hello" >>= \x->
  fmap (\y->x) [1..3]

  (iv)
  "hello" >>= \x->
  map (\y->x) [1..3]

  (v)
  (flip concatMap)
    "hello"
    \x -> map (\y->x) [1..3]
  
  (vi)
  concat  [ "hhh" 
          , "eee"
          , "lll"
          , "lll"
          , "ooo"
          ]


-}
