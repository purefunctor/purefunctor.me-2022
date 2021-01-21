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
  "blog" :> Capture "post-name" Text :> Get '[JSON] BlogPost


type RepoAPI =
  "repo" :> Capture "repo-name" Text :> Get '[JSON] Repository


type SiteAPI = BlogAPI :<|> RepoAPI


getBlogPost :: ConnectionPool -> Text -> IO (Maybe BlogPost)
getBlogPost pool postName = flip runSqlPersistMPool pool $ do
    post <- selectFirst [ BlogPostShortTitle ==. postName ] [ ]
    return $ case post of
      (Just post') -> Just . entityVal $ post'
      Nothing      -> Nothing


getBlogPostH :: ConnectionPool -> Text -> Handler BlogPost
getBlogPostH pool postName = do
    post <- liftIO $ getBlogPost pool postName
    case post of
       (Just post') -> return post'
       Nothing      -> throwError err404


getRepoInfo :: ConnectionPool -> Text -> IO (Maybe Repository)
getRepoInfo pool repoName = flip runSqlPersistMPool pool $ do
  repo <- selectFirst [ RepositoryName ==. repoName ] [ ]
  return $ case repo of
    (Just repo') -> Just . entityVal $ repo'
    Nothing      -> Nothing


getRepoInfoH :: ConnectionPool -> Text -> Handler Repository
getRepoInfoH pool repoName = do
  repo <- liftIO $ getRepoInfo pool repoName
  case repo of
    (Just repo') -> return repo'
    Nothing      -> throwError err404


siteAPI :: Proxy SiteAPI
siteAPI = Proxy


siteServer :: ConnectionPool -> Server SiteAPI
siteServer pool = getBlogPostH pool :<|> getRepoInfoH pool


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
        insert $ Repository "amalgam-lisp" "PureFunctor" "https://github.com/PureFunctor/amalgam-lisp" 3 453

    return $ siteApp pool


debug :: IO ()
debug = run 3000 =<< mkSiteApp
