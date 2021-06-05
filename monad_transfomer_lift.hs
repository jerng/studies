import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource

main :: IO (ResourceT IO String) 
main = return f

f :: ResourceT IO String 
f = lift io

io :: IO String
io = return "abc"
