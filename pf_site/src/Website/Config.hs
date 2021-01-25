module Website.Config where


import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (Text, pack)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool)
import System.Environment (getEnv)


data Configuration = Configuration
  { jwtSecret  :: Text
  , connection :: ConnectionPool
  } deriving Show


mkConfiguration :: IO Configuration
mkConfiguration = do
  jwt <- pack <$> getEnv "JWT_SECRET"
  
  dbn <- pack <$> getEnv "DB_FILE"
  dbc <- read <$> getEnv "DB_CONN"
  
  con <- runStderrLoggingT $ createSqlitePool dbn dbc
  
  return $ Configuration jwt con
