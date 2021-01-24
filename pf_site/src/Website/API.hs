{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API where

import Control.Monad (void)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Proxy (Proxy)
import Data.Time (getCurrentTime)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (run)
import Servant
import Website.API.Blog
import Website.API.Repo
import Website.Models


type WebsiteAPI = BlogPostAPI :<|> RepositoryAPI


websiteServer :: ConnectionPool -> Server WebsiteAPI
websiteServer pool = blogPostServer pool :<|> repositoryServer pool


websiteApp :: ConnectionPool -> Application
websiteApp = serve (Proxy :: Proxy WebsiteAPI) . websiteServer


mkWebsiteApp_ :: IO (ConnectionPool, Application)
mkWebsiteApp_ = do
  pool <- runStderrLoggingT $ createSqlitePool "database.sqlite" 2
  return (pool, websiteApp pool)


mkWebsiteApp :: IO Application
mkWebsiteApp = snd <$> mkWebsiteApp_


debug :: IO ()
debug = do
  (pool, app) <- mkWebsiteApp_

  now <- getCurrentTime

  flip runSqlPool pool $ do
    runMigration migrateAll
    insert $ BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" now now
    insert $ BlogPost "Python Is Awesome" "python-is-awesome" "SOON™" now now
    insert $ Repository "amalgam-lisp" "PureFunctor" "https://github.com/PureFunctor/amalgam-lisp" 0 0
    insert $ Repository "purefunctor.me" "PureFunctor" "https://github.com/PureFunctor/purefunctor.me" 0 0

  void $ run 3000 app
