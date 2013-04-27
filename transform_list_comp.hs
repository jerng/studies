{-

New keywords: "group", "by", "using"

New comprehension qualifiers:
1. then f
2. then f by e
3. then group by e using f
4. then group using f
5.

-}
{-# LANGUAGE TransformListComp #-}

import GHC.Exts

main = return output

output = 
  [ (the dept, sum salary)
  | (name, dept, salary) <- data_
  , then group by dept using groupWith
  --, then sortWith by (sum salary)
  , then take 5 
  ]

--output = [ (the dept, sum salary)
-- | (name, dept, salary) <- employees
--, then group by dept using groupWith
--, then sortWith by (sum salary)
--, then take 5 ]

data_ = 
  [ ("Simon", "MS", 80)
  , ("Erik", "MS", 100)
  , ("Phil", "Ed", 40)
  , ("Gordon", "Ed", 45)
  , ("Paul", "Yale", 60)
  ]

