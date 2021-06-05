--{-# LANGUAGE OverloadedStrings #-}

    --import qualified Data.Functor.Identity as F
    --import qualified Data.Typeable as Ty
    --
    --import Data.Bson
    --
    -- | MISUNDERSTOOD (cannot compile):
    -- |
    -- | Probably something about monads/functors:
    -- cast ((Int32 1) :: Value) :: (Maybe Int)

    -- | UNDERSTOOD (code runs):
    -- return.show $ typeOfVal (Int32 1) -- "Int32"
    -- return.show $ Ty.typeOf (Int32 1) -- "Value"
    -- return.show $ Ty.typeOf (1 :: Int) -- "Int"
    -- return.show $ (typed (Int32 1) :: Int) -- "1"

-- the Maybe typeclass
strDefault = "default_string"
fnAddString a = a ++ "_with_added_string"
datum1 = maybe strDefault fnAddString (Nothing) 
datum2 = maybe strDefault fnAddString (Just "some_string") 

-- the Either typeclass
fnLeftAction a = a ++ "_with_string_from_left_fork"
fnRightAction a = a ++ "_with_string_from_right_fork"
datum3 = either fnLeftAction fnRightAction (Right "some_string")
datum4 = either fnLeftAction fnRightAction (Left "some_string")

-- A FEW WAYS to do the same thing: 
-- case/of, let/in, where, new guard, list comprehensions
--------------------------------------------------------------------------------
--test a = J.mongoDBPrint $ case M.valueAt "results" a of M.Array b -> head b
--------------------------------------------------------------------------------
--test a = let M.Array b = M.valueAt "results" a in  J.mongoDBPrint $ head b 
--------------------------------------------------------------------------------
--test a = J.mongoDBPrint $ head b where M.Array b =  M.valueAt "results" a
--------------------------------------------------------------------------------
--test a | M.Array b <- M.valueAt "results" a = J.mongoDBPrint $ head b 
--------------------------------------------------------------------------------
-- test a = J.mongoDBPrint $ head [ b | M.Array b <- [M.valueAt "results" a]] 
--------------------------------------------------------------------------------

-- A FEW WAYS to do the same thing: 
-- functional (lispy), list comprehension, do notation (monadic, less sugar)
--------------------------------------------------------------------------------
--  do { pDoc <- universe ; let { a = test3 subject pDoc } ; return a }
--------------------------------------------------------------------------------
--  (map (test3 subject) universe :: [M.Value])
--------------------------------------------------------------------------------
--  [ a | pDoc <- universe, let a = test3 subject pDoc ]
--------------------------------------------------------------------------------
-- runghc THISFILE would fire this function:
--
main = 
  do
  a <- putStrLn "z"
  case a of
    a -> print a
