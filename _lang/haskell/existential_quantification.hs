{-# LANGUAGE ExistentialQuantification #-}


-- This declaration reads as "data of the datatype `A` is constructed
-- by writing `A [a]` where `a` is any type which instantiates the
-- `Show` typeclass.
data A = 
  forall a. -- this line is required; otherwise, this will not compile
  Show a => A [a]



main = return 1
