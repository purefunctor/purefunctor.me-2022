{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API where


import Data.Proxy
import Data.Time
import Data.Time.Calendar.Julian
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server
import Website.Models


type BlogAPI =
  "blog" :> Capture "blogid" Int :> Get '[JSON] BlogPost


blogPostZero :: BlogPost
blogPostZero = BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" timeZero timeZero
  where
    timeZero = UTCTime (fromJulian 2020 25 12) (secondsToDiffTime 0)


handleBlogPost :: Int -> Handler BlogPost
handleBlogPost _ = return blogPostZero


blogAPI :: Proxy BlogAPI
blogAPI = Proxy


blogServer :: Server BlogAPI
blogServer = return (handleBlogPost 0)


blogApp :: Application
blogApp = serve blogAPI blogServer
