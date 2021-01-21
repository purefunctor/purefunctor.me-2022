{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API where


import Control.Monad.IO.Class
import Data.Proxy
import Data.Time
import Data.Time.Calendar.Julian
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Website.Models


type BlogAPI =
  "blog" :> Capture "blogid" Int :> Get '[JSON] BlogPost


handleBlogPost :: Handler BlogPost
handleBlogPost = do
  time <- liftIO getCurrentTime
  return $ BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" time time


blogAPI :: Proxy BlogAPI
blogAPI = Proxy


blogServer :: Server BlogAPI
blogServer = return handleBlogPost


blogApp :: Application
blogApp = serve blogAPI blogServer
