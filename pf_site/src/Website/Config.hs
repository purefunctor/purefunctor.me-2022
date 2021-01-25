module Website.Config where


import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (Text, pack)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool)
import System.Environment (getEnv)


data Configuration = Configuration
  { jwtSecret  :: Text
  , adminUser  :: Text
  , adminPass  :: Text
  , connection :: ConnectionPool
  } deriving Show


mkConfiguration :: IO Configuration
mkConfiguration = do
  jwt <- pack <$> getEnv "JWT_SECRET"
  
  adu <- pack <$> getEnv "ADMIN_USER"
  adp <- pack <$> getEnv "ADMIN_PASS"
  
  dbn <- pack <$> getEnv "DB_FILE"
  dbc <- read <$> getEnv "DB_CONN"
  
  con <- runStderrLoggingT $ createSqlitePool dbn dbc
  
  return $ Configuration jwt adu adp con
