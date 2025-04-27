-- BROKEN
{-# LANGUAGE OverloadedStrings #-} 

import Data.Dynamic
import Data.ByteString

main = do   let a = appCVars
            return a

data AppCVars
    =   AppVar1 
    |   AppVar2
    deriving (Show)

appCVars :: [(AppCVars,Dynamic)]
appCVars = [
        (   AppVar1,
            toDyn ("\"Hello, I'm defined in AppController\"" :: ByteString))
        ,(  AppVar2,
            toDyn ("\"Hello, I too am defined in AppController\"" :: ByteString))
    ]
