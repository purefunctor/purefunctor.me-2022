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


blogAPI :: Proxy BlogAPI
blogAPI = Proxy


blogServer :: ConnectionPool -> Server BlogAPI
blogServer = getBlogPostH


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
