-- example from the web

import Data.Dynamic
import Data.Maybe
 
hlist :: [Dynamic]
hlist = [toDyn "string",
         toDyn (7::Int),
         toDyn (pi :: Double),
         toDyn 'x',
         toDyn ((), Just "foo")]
 
dyn :: Dynamic
dyn = hlist !! 1
 
v :: Int
v = case fromDynamic dyn of
         Nothing -> error "Type mismatch"
         Just x -> x
 
main = do putStrLn $ show v
