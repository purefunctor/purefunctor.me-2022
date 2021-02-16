module Website.Config where

import Control.Lens ( makeFieldsNoPrefix, (^.) )

import Control.Monad.Logger ( runStderrLoggingT )

import Data.Text ( Text )

import Database.Persist.Sqlite ( ConnectionPool, createSqlitePool )

import           Toml ( TomlCodec, (.=) )
import qualified Toml


data AdminCreds
  = AdminCreds
      { _username :: Text
      , _password :: Text
      }
  deriving (Eq, Show)

makeFieldsNoPrefix ''AdminCreds


data DatabaseConfig
  = DatabaseConfig
      { _filename    :: Text
      , _connections :: Int
      }
  deriving (Eq, Show)

makeFieldsNoPrefix ''DatabaseConfig


data GitHubCreds
  = GitHubCreds
      { _username :: Text
      , _token    :: Text
      }
  deriving (Eq, Show)

makeFieldsNoPrefix ''GitHubCreds


data ConfigFile
  = ConfigFile
      { _admin    :: AdminCreds
      , _database :: DatabaseConfig
      , _github   :: GitHubCreds
      }
  deriving (Eq, Show)

makeFieldsNoPrefix ''ConfigFile


adminCredsCodec :: TomlCodec AdminCreds
adminCredsCodec = AdminCreds
  <$> Toml.text "username" .= (_username :: AdminCreds -> Text)
  <*> Toml.text "password" .= _password


databaseConfigCodec :: TomlCodec DatabaseConfig
databaseConfigCodec = DatabaseConfig
  <$> Toml.text "filename"    .= _filename
  <*> Toml.int  "connections" .= _connections


githubCredsCodec :: TomlCodec GitHubCreds
githubCredsCodec = GitHubCreds
  <$> Toml.text "username" .= (_username :: GitHubCreds -> Text)
  <*> Toml.text "token"    .= _token


configFileCodec :: TomlCodec ConfigFile
configFileCodec = ConfigFile
  <$> Toml.table adminCredsCodec     "admin"    .= _admin
  <*> Toml.table databaseConfigCodec "database" .= _database
  <*> Toml.table githubCredsCodec    "github"   .= _github


data Environment
  = Environment
      { _config :: ConfigFile
      , _pool   :: ConnectionPool
      }
  deriving (Show)

makeFieldsNoPrefix ''Environment


mkEnvironment :: IO Environment
mkEnvironment = do
  conf <- Toml.decodeFile configFileCodec "config.toml"

  pool' <- runStderrLoggingT $
    createSqlitePool (conf^.database.filename) (conf^.database.connections)

  return $ Environment conf pool'
