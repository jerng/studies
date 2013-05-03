{-# LANGUAGE  OverloadedStrings
            , FlexibleContexts
  #-}

module AppModel where

import qualified Data.Text as T
import qualified Database.MongoDB as M
import Hell.Lib

main :: IO (Either M.Failure [M.Document])
main = do
  let -- Same for all queries to the database
      hostname    = "127.0.0.1"       -- :: Network.Socket.HostName
      host        = M.host hostname   :: M.Host
      db          = "testdb"          :: M.Database
      collection  = "testcollection"  :: M.Collection
      accessmode  = M.master          :: M.AccessMode
          -- omg. mongoDB-1.3.2 provides three, but there are five here:
          -- http://docs.mongodb.org/manual/core/read-preference/
      
      -- Varies according to query to the database.
      -- Can these be written in Pure Code?
      selector    = []                :: M.Selector
      query   = M.select selector collection :: M.Query
      action  = M.rest =<< 
                ( M.find query 
                --  :: (MonadIO m, MonadBaseControl IO m) => M.Action m M.Cursor
                )
                --  :: (MonadIO m, MonadBaseControl IO m, Functor m) 
                --  => M.Action m [M.Document]

  pipe          <- M.runIOE (M.connect host :: M.IOE M.Pipe) :: IO M.Pipe
  eitherResult  <- M.access pipe accessmode db action 
                   :: IO (Either M.Failure [M.Document])
  return eitherResult
