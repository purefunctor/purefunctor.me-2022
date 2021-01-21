{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API where


import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Proxy
import Data.Time
import Data.Time.Calendar.Julian
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Server
import Website.Models


type BlogAPI =
  "blog" :> Capture "blogid" Int :> Get '[JSON] BlogPost



getBlogPost :: ConnectionPool -> IO (Maybe BlogPost)
getBlogPost pool = flip runSqlPersistMPool pool $ do
    post <- selectFirst [ BlogPostShortTitle ==. "haskell-is-simple" ] [ ]
    return $ case post of
      (Just post') -> Just . entityVal $ post'
      Nothing      -> Nothing


getBlogPostH :: ConnectionPool -> Handler BlogPost
getBlogPostH pool = do
    post <- liftIO $ getBlogPost pool
    case post of
       (Just post') -> return post'
       Nothing      -> throwError err404


blogAPI :: Proxy BlogAPI
blogAPI = Proxy


blogServer :: ConnectionPool -> Server BlogAPI
blogServer = return . getBlogPostH


blogApp :: ConnectionPool -> Application
blogApp = serve blogAPI . blogServer


mkBlogApp :: IO Application
mkBlogApp = do
    pool <- runStderrLoggingT $ do
        createSqlitePool "db.sqlite" 5

    time <- getCurrentTime

    flip runSqlPool pool $ do
        runMigration migrateAll
        insert $ BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" time time

    return $ blogApp pool


debug :: IO ()
debug = run 3000 =<< mkBlogApp
