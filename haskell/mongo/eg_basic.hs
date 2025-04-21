-- example from basic docs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Database.MongoDB
import Control.Monad.Trans (liftIO)

main = do
  pipe <- runIOE $ connect (host "127.0.0.1")
     -- runIOE, connect : from Database.MongoDB.Connection
  e <- access pipe master "teams" run
     -- access, master : from Database.MongoDB.Query
     -- pipe : from above
     -- run : from this file
  close pipe
     -- close : from Database.MongoDB.Connection
  print e

run = do
  authTeams
  clearTeams
  insertTeams
  allTeams >>= printDocs "All Teams"
  nationalLeagueTeams >>= printDocs "National League Teams"
  newYorkTeams >>= printDocs "New York Teams"
     -- *Teams, printDocs : from this file 

authTeams = auth "hs_mongo_user" "hs_mongo_user"

clearTeams = delete (select [] "team")
     -- delete : from Database.MongoDB.Query

insertTeams = insertMany "team" [
  [  "name" =: "Yankees", 
     "home" =: ["city" =: "New York", "state" =: "NY"], 
     "league" =: "American"],
  [  "name" =: "Mets", 
     "home" =: ["city" =: "New York", "state" =: "NY"], 
     "league" =: "National"],
  [  "name" =: "Phillies", 
     "home" =: ["city" =: "Philadelphia", "state" =: "PA"], 
     "league" =: "National"],
  [  "name" =: "Red Sox", 
     "home" =: ["city" =: "Boston", "state" =: "MA"], 
     "league" =: "American"] ]
     -- insertMany: from Database.MongoDB.Query

allTeams = rest =<< find (select [] "team") {sort = ["home.city" =: 1]}
     -- rest, find, select, sort : from Database.MongoDB.Query

nationalLeagueTeams = rest =<< find (select ["league" =: "National"] "team")
     -- ditto 

newYorkTeams = rest =<< find  (select ["home.state" =: "NY"] "team") 
                             {project = ["name" =: 1, "league" =: 1]}
     -- project : from Database.MongoDb.Query 

printDocs title docs = 
 liftIO $ putStrLn title >> 
 mapM_ (print . exclude ["_id"]) docs
     -- lisftIO : from Control.Monad.Trans 
     -- mapM_ : from Data.Foldable
     -- print : from Prelude
     -- exclude : from Data.BSON
     


{-
    --
    -- _actions is a monad, in the (main) IO monad;
    -- so (mapM_) needs to be (lift)ed out;
    -- (liftIO) would also work;
    --
    -- still unsure about (lift) vs (liftIO);
    -- ; figure out how to, instead of printing
    -- here, pass output to (main) (via another
    -- monad wrapper? un/curry?) and print 
    -- everything at once, at the end of (main).
    --
    -- Look also into instances of the Show typeclass.


                               liftIO :: IO a -> m a
                                           ^
                                           |
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
                           ^
                           |
print :: Show a => a -> IO ()
                   ^
                   |
                docs
-}
