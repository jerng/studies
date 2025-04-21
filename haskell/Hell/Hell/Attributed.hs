{-# LANGUAGE OverloadedStrings #-}
 
module Hell.Attributed  where

import Control.Monad (foldM)

-- MODULARISE: Hell.Parse.Forms
-- base-4.6.0.1 has (readMaybe), but meanwhile this is an alternative
-- from http://stackoverflow.com/posts/8067014/revisions
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

-- | FROM: http://stackoverflow.com/questions/15216621
--    /can-partition-be-applied-to-a-io-bool
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p xs = foldM f ([], []) xs
  where 
    f (a, b) x = do
      flag <- p x
      return $ if flag 
        then (x : a, b) 
        else (a, x : b)
