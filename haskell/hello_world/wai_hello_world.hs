{-# LANGUAGE OverloadedStrings #-} 

import Network
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
-------------------------------------------------------------------------------
application = \_-> return $ responseLBS status200 [("Content-Type", "text/plain")] 
                            "Hello World" 

main = withSocketsDo $ do run 3000 application


{-

run ::
  warp-1.3.2:Network.Wai.Handler.Warp.Types.Port
  -> Application -> IO ()
        -- Defined in `warp-1.3.2:Network.Wai.Handler.Warp.Run'

type Application =
  Request -> Control.Monad.Trans.Resource.ResourceT IO Response
        -- Defined in `Network.Wai'

responseLBS ::
  Network.HTTP.Types.Status.Status
  -> Network.HTTP.Types.Header.ResponseHeaders
  -> Data.ByteString.Lazy.Internal.ByteString
  -> Response
        -- Defined in `Network.Wai'

status200 :: Network.HTTP.Types.Status.Status
        -- Defined in `Network.HTTP.Types.Status'

-}
