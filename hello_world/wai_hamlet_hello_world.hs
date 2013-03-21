{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad.Trans.Resource

import Text.Hamlet                                  as Hamlet

import qualified Data.ByteString.Lazy.Char8         as ByteString
import qualified Network.Wai                        as Wai
import qualified Network.HTTP.Types                 as Http
import qualified Network.Wai.Handler.Warp           as Warp

-- | Are these already called by Hamlet?
import qualified Text.Blaze                         as Blaze 
import qualified Text.Blaze.Html.Renderer.String    as Blaze
-------------------------------------------------------------------------------
main :: IO ()
main = Warp.run 3000 application

application :: t -> Control.Monad.Trans.Resource.ResourceT IO Wai.Response
application = \request -> return   $ Wai.responseLBS Http.ok200 [] 
                                    $ ByteString.pack 
                                    $ Blaze.renderHtml 
                                    $ template request

--template :: Hamlet.Html -- figure out the diff. between [|hamlet & [|shamlet
template request = [shamlet|
<!DOCTYPE html>
<html>
    <head>
        <script type="text/javascript" src="jquery.mobile-1.2.0.min.js" />
        <title>Study Graph
    <body>
        <h1>something
        #{x}
|]

x = "a" :: String
