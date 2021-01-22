{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API where


import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Text
import Data.Time
import Database.Persist.Sqlite
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import Website.Models


data PostInfo = PostInfo
  { title   :: Text
  , content :: Text
  } deriving Generic


instance FromJSON PostInfo
instance ToJSON PostInfo


data RepoInfo = RepoInfo
  { name  :: Text
  , owner :: Text
  } deriving Generic


instance FromJSON RepoInfo
instance ToJSON RepoInfo


type BlogAPI =
  "blog" :> "all-posts" :> Get '[JSON] [BlogPost] :<|>
  "blog" :> "create" :> ReqBody '[JSON] PostInfo :> Post '[JSON] BlogPost :<|>
  "blog" :> Capture "post-name" Text :> Get '[JSON] BlogPost


type RepoAPI =
  "repo" :> "all-repos" :> Get '[JSON] [Repository] :<|>
  "repo" :> "create" :> ReqBody '[JSON] RepoInfo :> Post '[JSON] Repository :<|>
  "repo" :> Capture "repo-name" Text :> Get '[JSON] Repository


type SiteAPI = BlogAPI :<|> RepoAPI


getBlogPostH :: ConnectionPool -> Text -> Handler BlogPost
getBlogPostH pool postName = do
  post <- liftIO $ flip runSqlPersistMPool pool $
    selectFirst [ BlogPostShortTitle ==. postName ] [ ]

  case post of
    (Just post') -> return $ entityVal post'
    Nothing      -> throwError err404


getAllPostsH :: ConnectionPool -> Handler [BlogPost]
getAllPostsH pool = do
  posts <- liftIO $ flip runSqlPersistMPool pool $
    selectList [ ] [ ]
  return $ entityVal <$> posts


postBlogPostH :: ConnectionPool -> PostInfo -> Handler BlogPost
postBlogPostH pool info = do
  now <- liftIO getCurrentTime

  let title'   = title info
  let short    = Data.Text.replace " " "_" title'
  let content' = content info

  let post     = BlogPost title' short content' now now

  _ <- liftIO $ flip runSqlPersistMPool pool $ insert post

  return post


getRepoInfoH :: ConnectionPool -> Text -> Handler Repository
getRepoInfoH pool repoName = do
  repo <- liftIO $ flip runSqlPersistMPool pool $
    selectFirst [ RepositoryName ==. repoName ] [ ]

  case repo of
    (Just repo') -> return $ entityVal repo'
    Nothing      -> throwError err404


getAllReposH :: ConnectionPool -> Handler [Repository]
getAllReposH pool = do
  repos <- liftIO $ flip runSqlPersistMPool pool $
    selectList [ ] [ ]
  return $ entityVal <$> repos


postRepoInfoH :: ConnectionPool -> RepoInfo -> Handler Repository
postRepoInfoH pool info = do
  let name'  = name info
  let owner' = owner info
  let url    = Data.Text.concat [ "https://github.com" , "/" , name' , "/" , owner' ]
  let repo   = Repository name' owner' url 0 0

  _ <- liftIO $ flip runSqlPersistMPool pool $ insert repo

  return repo


siteAPI :: Proxy SiteAPI
siteAPI = Proxy


siteServer :: ConnectionPool -> Server SiteAPI
siteServer pool = blogServer :<|> repoServer
  where
    blogServer = getAllPostsH pool :<|> postBlogPostH pool :<|> getBlogPostH pool
    repoServer = getAllReposH pool :<|> postRepoInfoH pool :<|> getRepoInfoH pool


siteApp :: ConnectionPool -> Application
siteApp = serve siteAPI . siteServer


mkSiteApp :: IO Application
mkSiteApp = do
  pool <- runStderrLoggingT $
    createSqlitePool "db.sqlite" 5

  time <- getCurrentTime

  flip runSqlPool pool $ do
    runMigration migrateAll
    insert $ BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" time time
    insert $ BlogPost "Python Is Awesome" "python-is-awesome" "SOON™" time time
    insert $ Repository "amalgam-lisp" "PureFunctor" "https://github.com/PureFunctor/amalgam-lisp" 3 453
    insert $ Repository "purefunctor.me" "PureFunctor" "https://github.com/PureFunctor/purefunctor.me" 3 149

  return $ siteApp pool


debug :: IO ()
debug = run 3000 =<< mkSiteApp
