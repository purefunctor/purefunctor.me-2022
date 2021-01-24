{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API where


import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Database.Persist.Sqlite
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Website.Models


type BlogPostAPI =
  "post" :> Get '[JSON] [BlogPost] :<|>
  "post" :> Capture "short-title" Text :> Get '[JSON] BlogPost :<|>
  "post" :> ReqBody '[JSON] CreateBlogPostData :> Post '[JSON] BlogPost


data CreateBlogPostData = CreateBlogPostData
  { title   :: Text
  , content :: Text
  } deriving (Generic, FromJSON, ToJSON)


blogPostServer :: ConnectionPool -> Server BlogPostAPI
blogPostServer pool = getPosts :<|> getPost :<|> mkPost
  where
    getPosts :: Handler [BlogPost]
    getPosts = do
      posts <- liftIO $ flip runSqlPersistMPool pool $
        selectList [ ] [ ]
      return $ entityVal <$> posts

    getPost :: Text -> Handler BlogPost
    getPost t = do
      post <- liftIO $ flip runSqlPersistMPool pool $
        selectFirst [ BlogPostShortTitle ==. t ] [ ]
      case post of
        (Just post') -> return $ entityVal post'
        Nothing -> throwError err404

    mkPost :: CreateBlogPostData -> Handler BlogPost
    mkPost (CreateBlogPostData title content) = do
      now <- liftIO getCurrentTime

      let short = Text.intercalate "_"
                . take 3
                . Text.words
                $ title
                
      let post  = BlogPost title short content now now

      void $ liftIO $ flip runSqlPersistMPool pool $ insert post
      
      return post


type RepositoryAPI =
  "repo" :> Get '[JSON] [Repository] :<|>
  "repo" :> Capture "name" Text :> Get '[JSON] Repository :<|>
  "repo" :> ReqBody '[JSON] CreateRepositoryData:> Post '[JSON] Repository


data CreateRepositoryData = CreateRepositoryData
  { name    :: Text
  , owner   :: Text
  } deriving (Generic, FromJSON, ToJSON)


repositoryServer :: ConnectionPool -> Server RepositoryAPI
repositoryServer = undefined



type WebsiteAPI = BlogPostAPI :<|> RepositoryAPI


websiteServer :: ConnectionPool -> Server WebsiteAPI
websiteServer pool = blogPostServer pool :<|> repositoryServer pool


websiteApp :: ConnectionPool -> Application
websiteApp = serve (Proxy :: Proxy WebsiteAPI) . websiteServer


mkWebsiteApp_ :: IO (ConnectionPool, Application)
mkWebsiteApp_ = undefined


mkWebsiteApp :: IO Application
mkWebsiteApp = snd <$> mkWebsiteApp_


debug :: IO ()
debug = do
  (pool, app) <- mkWebsiteApp_
  return ()
