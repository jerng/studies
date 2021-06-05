-- refer to standalone tex_journal for cleaner code
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
-------------------------------------------------------------------------------
application :: Wai.Request -> ResourceT IO Wai.Response
application request = 
    return 
    $ case (head $ Wai.pathInfo request) of

        "html" ->   Wai.responseLBS Http.status200 [] 
                    $ ByteString.pack 
                    $ Blaze.renderHtml htmlDoc 

        "css" ->    Wai.ResponseFile 
                    Http.status200 
                    [] 
                    "./FILENAME.css"
                    Nothing

        "js" ->     Wai.ResponseFile 
                    Http.status200 
                    [] 
                    "./jquery-1.8.3.min.js"
                    Nothing

        _ ->        Wai.responseLBS 
                    Http.status400 
                    [] 
                    "" -- optimised? 
-------------------------------------------------------------------------------
htmlDoc :: Hamlet.Html
htmlDoc = [shamlet|
<!DOCTYPE html>
<html>
    <head>
        <title>mini framework
        <link rel="stylesheet" type="text/css" href="/css">
        <script type="text/javascript" src="/js" />
        <style>
            h1  {   color: red  }
    <body>
        <h1>something µ 
        <script>$('h1').fadeOut().fadeIn()
|]
-------------------------------------------------------------------------------
