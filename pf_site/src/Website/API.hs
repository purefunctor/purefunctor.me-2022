{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API where

import Control.Monad (void)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Proxy (Proxy)
import Data.Time (getCurrentTime)
import Database.Persist.Sqlite
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Website.API.Blog
import Website.API.Repo
import Website.Config
import Website.Models
import Website.WebsiteM


type WebsiteAPI = BlogPostAPI :<|> RepositoryAPI


websiteServer :: ServerT WebsiteAPI WebsiteM
websiteServer = blogPostServer :<|> repositoryServer


websiteApp :: Configuration -> Application
websiteApp config = serve api $ hoistServer api (runWebsiteM config) websiteServer
  where
    api :: Proxy WebsiteAPI
    api = Proxy


mkWebsiteApp_ :: IO (Configuration, Application)
mkWebsiteApp_ = do
  config <- mkConfiguration
  return (config, websiteApp config)
  

mkWebsiteApp :: IO Application
mkWebsiteApp = snd <$> mkWebsiteApp_


debug :: IO ()
debug = do
  (Configuration _ _ _ pool, app) <- mkWebsiteApp_

  now <- getCurrentTime

  flip runSqlPool pool $ do
    runMigration migrateAll
    insert $ BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" now now
    insert $ BlogPost "Python Is Awesome" "python-is-awesome" "SOON™" now now
    insert $ Repository "amalgam-lisp" "PureFunctor" "https://github.com/PureFunctor/amalgam-lisp" 0 0
    insert $ Repository "purefunctor.me" "PureFunctor" "https://github.com/PureFunctor/purefunctor.me" 0 0

  void $ run 3000 app
