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
application request = return $
    case (head $ Wai.pathInfo request) of
        "html" ->   
            Wai.responseLBS Http.status200  []  $ ByteString.pack 
                                                $ Blaze.renderHtml 
                                                $ htmlDoc
        "d3" ->     
            Wai.ResponseFile Http.status200 [] "./d3.v2.min.js" Nothing
        _ ->        
            Wai.responseLBS Http.status400  [] "" -- optimised? 
-------------------------------------------------------------------------------
htmlDoc :: Hamlet.Html
htmlDoc = [shamlet|
!!!
<html>
    <head>
        <title>Study Graph
        <!-- <link rel="stylesheet" type="text/css" href="/css"> -->
        <script type="text/javascript" src="/d3" />
        <style>
        <script>
            window.onload = function()
            {
                svg = 
                    d3  .select("body")
                        .append("svg")
                        .attr("width", "100%")
                        .attr("height", "100%")
                \
                c = 
                    svg .selectAll("circle")
                        .data([ {"cx": 10, "cy": 10, "r":5}, 
                                {"cx": 20, "cy": 20, "r":10} ])
                        .enter()
                        .append("circle")
                \
                c   
                    .attr("r",function(d,i){ return d.r})
                    .attr("cx",function(d,i){ return d.cx})
                    .attr("cy",function(d,i){ return d.cy})
                    
                    .transition()
                    .attr("cy",function(d,i){ return (d.cy * 5)})
                    .each("end",function()
                        {   d3  .select(this)
                                .transition()
                                .attr("cy",function(d,i){ return (d.cy * 3)})

                        })
                
            }
    <body>
|]
-------------------------------------------------------------------------------
