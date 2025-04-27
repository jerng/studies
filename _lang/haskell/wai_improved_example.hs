{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE QuasiQuotes #-}

import qualified Network.Wai            as Wai

import Network.HTTP.Types               (status200) 
import Network.Wai.Handler.Warp         (run) 
import Control.Monad.Trans.Resource     (ResourceT)
import Data.Text                        (Text)
import Data.ByteString.UTF8             (toString)
import Text.Blaze.Renderer.Utf8         (renderMarkupBuilder)
import Text.Hamlet                      (   hamletFile
                                            ,shamletFile
                                            ,Html
                                            ,HtmlUrl 
                                        )
--import Data.ByteString.Lazy.UTF8        (fromString)
--import Text.Blaze.Html.Renderer.Utf8    (renderHtml)

data AppUrls = GoogleUrl

-------------------------------------------------------------------------------
renderAppUrls :: AppUrls -> [(Text, Text)] -> Text
renderAppUrls GoogleUrl _ = "http://www.google.com"

-------------------------------------------------------------------------------
main :: IO ()
main = run 3000 application

-------------------------------------------------------------------------------
application :: Wai.Request -> ResourceT IO Wai.Response

application request

    -- no path directories were specified
    | []    <- Wai.pathInfo request 
    =   return 
        $   Wai.ResponseBuilder
            status200 
            [] 
            $   renderMarkupBuilder 
                $   indexView 
                    renderAppUrls

    -- path directories were specified
    | a:_   <- Wai.pathInfo request 
    , b     <- "static" ++ (toString $ Wai.rawPathInfo request)
    = return $ Wai.ResponseFile status200 [] b Nothing

-------------------------------------------------------------------------------
indexView :: HtmlUrl AppUrls
indexView = $(hamletFile "index.hamlet")

-------------------------------------------------------------------------------
widgetView :: HtmlUrl AppUrls
widgetView = $(hamletFile "widget.hamlet")

-------------------------------------------------------------------------------
variable :: Text
variable = " {{ an attempt to insert a variable }} "

-------------------------------------------------------------------------------

