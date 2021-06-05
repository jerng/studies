{-# LANGUAGE OverloadedStrings #-}

-- Six  varieties of naming in Haskell:
--
-- Lexemes start in lower case:
-- 1. data variables (first-order entities: have a type )
-- 2. type variables 
--
-- Lexemes start in upper case:
-- 3. modules
-- 4. data constructors (first-order entities: have a type
-- 5. types
-- 6. typeclasses 












-- Some function declarations followed by their type explicit signatures:
--
fnNullary     = "returned by fnNullary"
fnNullary     :: String

fnUnary       :: a -> String
fnUnary _     = "returned by fnUnary"

fnBinary _ _  = "returned by fnBinary"
fnBinary      :: a -> b -> String

_ `fnInfix` _ = "returned by fnInfix"
fnInfix       :: a -> b -> String

-- the typeclass syntax here is called the "typeclass context of a"
--
fnGuardedUnary x 
  | x < 0   = "returned by fnGuardedUnary branch: < 0" 
  | x == 0  = "returned by fnGuardedUnary branch: == 0" 
  | x > 0   = "returned by fnGuardedUnary branch: > 0" 
fnGuardedUnary :: (Num a, Eq a, Ord a) => a -> String













-- Some "algebraic data type" declarations followed by their instantiation:
--
data TypeCon1 = DataCon1 | DataCon2 
                -- type values are also called "data constructors"
                deriving (Show)
                -- the typeclass syntax here "makes TypeCon1 an instance of Show"
datum1 = DataCon1
datum2 = DataCon2

-- "polymorphic" algebraic data types, can have parameters; 
-- arguments must be types
data TypeCon2 typeVar1 typeVar2 typeVar3 =    DataCon3 typeVar1 
                                            | DataCon4 typeVar2 typeVar3 
                                            deriving (Show)
datum3 = DataCon3 "lala1" 
datum4 = DataCon3 "lala2"
datum5 = DataCon4 "lala3" "lala4"

-- (side note) Some aliasing examples (some declarations):
type S1 = TypeCon2
type S2 = String

-- "record syntax" gives us accessor functions to reference arguments of a
-- polymorphic algebraic data type by name, instead of by position
data TypeCon3 = DataCon5 {  fieldName1,
                            fieldName2,
                            fieldName3 :: Int
                                       -- specifies all fields as type Int
                         } deriving Show
data TypeCon4 = DataCon6 {  fieldName4 :: String,
                            fieldName5 :: Float,
                            fieldName6 :: Int
                         } deriving Show
datum6 = DataCon5 {fieldName1 = 1, fieldName2 = 2, fieldName3 = 3}
datum7 = DataCon6 {fieldName4 = "hi there", fieldName5 = 2.1, fieldName6 = 3}
datum8 = DataCon6 {fieldName4 = "hi ho", fieldName5 = 2.2, fieldName6 = 4}

-- (side note) pattern matching with records:
fnHasHiHo DataCon6 { fieldName4 = "hi ho" }  = True
fnHasHiHo _                                  = False
datum9  = fnHasHiHo datum7
datum10 = fnHasHiHo datum8











-- some "typeclass" definitions, followed by their instantiation:
--
class Class1 typeVar4 where
  fnInstance :: typeVar4 -> String
  -- only type signatures of functions are provided here

instance Class1 Int where
  fnInstance _ = "returned by fnInstance (Int argument)"

instance Class1 [a] where
  fnInstance _ = "returned by fnInstance (String argument)"













-- runghc THISFILE would fire this function:
--
main = 
  do
  putStrLn ("")
  putStrLn ("printed: '" ++ fnNullary ++ "'")
  putStrLn ("printed: '" ++ fnUnary "ignored" ++ "'")
  putStrLn ("printed: '" ++ fnBinary "ignored" "ignored" ++ "'")
  putStrLn ("printed: '" ++ "ignored" `fnInfix` "ignored" ++ "'")
  putStrLn ("printed: '" ++ fnGuardedUnary (-1) ++ "'")
  putStrLn ("printed: '" ++ fnGuardedUnary 0 ++ "'")
  putStrLn ("printed: '" ++ fnGuardedUnary (-1) ++ "'")

  -- note the type annotations for datum3, datum4
  --
  putStrLn ("")
  putStrLn ("printed: 'show datum1' returned '" ++ show datum1 ++ "'")
  putStrLn ("printed: 'show datum2' returned '" ++ show datum2 ++ "'")
  putStrLn ("printed: 'show datum3' returned '" ++ 
    show (datum3 :: TypeCon2 String String String) ++ "'" )
  putStrLn ("printed: 'show datum4' returned '" ++ 
    show (datum4 :: TypeCon2 String String String) ++ "'" )
  putStrLn ("printed: 'show datum5' returned '" ++ 
    show (datum5 :: S1 S2 S2 S2) ++ "'" ) -- An aliasing example (a use case).
  putStrLn ("printed: 'show datum6' returned '" ++ show datum6 ++ "'")
  putStrLn ("printed: 'show datum7' returned '" ++ show datum7 ++ "'")
  putStrLn ("printed: 'show datum8' returned '" ++ show datum8 ++ "'")
  putStrLn ("printed: 'show datum9' returned '" ++ show datum9 ++ "'")
  putStrLn ("printed: 'show datum10' returned '" ++ show datum10 ++ "'")

  putStrLn ("")
  putStrLn ("printed: '" ++ fnInstance (1 :: Int) ++ "'")
  putStrLn ("printed: '" ++ fnInstance ("some string" :: String) ++ "'")
  putStrLn ("")
