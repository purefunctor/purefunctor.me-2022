{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API where


import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Proxy
import Data.Text
import Data.Time
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp
import Servant
import Website.Models


type BlogAPI =
  "blog" :> "all-posts" :> Get '[JSON] [BlogPost] :<|>
  "blog" :> Capture "post-name" Text :> Get '[JSON] BlogPost


type RepoAPI =
  "repo" :> "all-repos" :> Get '[JSON] [Repository] :<|>
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


siteAPI :: Proxy SiteAPI
siteAPI = Proxy


siteServer :: ConnectionPool -> Server SiteAPI
siteServer pool = blogServer :<|> repoServer
  where
    blogServer = getAllPostsH pool :<|> getBlogPostH pool 
    repoServer = getAllReposH pool :<|> getRepoInfoH pool 


siteApp :: ConnectionPool -> Application
siteApp = serve siteAPI . siteServer


mkSiteApp :: IO Application
mkSiteApp = do
  pool <- runStderrLoggingT $ do
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
