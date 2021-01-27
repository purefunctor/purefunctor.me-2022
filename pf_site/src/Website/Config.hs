module Website.Config where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (Text, pack)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool)
import System.Environment (getEnv)


data Configuration = Configuration
  { adminUser :: Text
  , adminPass :: Text
  , connPool  :: ConnectionPool
  } deriving Show


mkConfiguration :: IO Configuration
mkConfiguration = do
  adu <- pack <$> getEnv "ADMIN_USER"
  adp <- pack <$> getEnv "ADMIN_PASS"
  
  dbn <- pack <$> getEnv "DB_FILE"
  dbc <- read <$> getEnv "DB_CONN"
  
  con <- runStderrLoggingT $ createSqlitePool dbn dbc
  
  return $ Configuration adu adp con
