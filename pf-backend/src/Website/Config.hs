module Website.Config where

import Control.Lens ( makeFieldsNoPrefix, makeLenses, (^.) )

import Control.Monad.Logger ( runStderrLoggingT )

import Data.Maybe ( fromMaybe )

import Data.Text ( Text )

import Database.Persist.Sqlite ( ConnectionPool, createSqlitePool )

import           Toml ( TomlCodec, (.=) )
import qualified Toml

import System.Environment.Blank ( getEnv )

import Paths_purefunctor_me ( getDataFileName )


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

makeLenses ''DatabaseConfig


data GitHubCreds
  = GitHubCreds
      { _username :: Text
      , _token    :: Text
      }
  deriving (Eq, Show)

makeFieldsNoPrefix ''GitHubCreds


newtype DebugConfig
  = DebugConfig
      { _static :: Bool
      }
  deriving (Eq, Show)

makeLenses ''DebugConfig


data ConfigFile
  = ConfigFile
      { _admin    :: AdminCreds
      , _database :: DatabaseConfig
      , _github   :: GitHubCreds
      , _debug    :: DebugConfig
      }
  deriving (Eq, Show)

makeLenses ''ConfigFile


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


debugConfigCodec :: TomlCodec DebugConfig
debugConfigCodec = DebugConfig
  <$> Toml.bool "static" .= _static


configFileCodec :: TomlCodec ConfigFile
configFileCodec = ConfigFile
  <$> Toml.table adminCredsCodec     "admin"    .= _admin
  <*> Toml.table databaseConfigCodec "database" .= _database
  <*> Toml.table githubCredsCodec    "github"   .= _github
  <*> Toml.table debugConfigCodec    "debug"    .= _debug


data Environment
  = Environment
      { _config :: ConfigFile
      , _pool   :: ConnectionPool
      }
  deriving (Show)

makeFieldsNoPrefix ''Environment


mkEnvironment :: IO Environment
mkEnvironment = do
  defaultConfig <- getDataFileName "config-default.toml"
  configFile <- fromMaybe defaultConfig <$> getEnv "CONFIG_FILE"
  conf <- Toml.decodeFile configFileCodec configFile

  pool' <- runStderrLoggingT $
    createSqlitePool (conf^.database.filename) (conf^.database.connections)

  return $ Environment conf pool'