{-# LANGUAGE ImpredicativeTypes #-}
module Website.Database
  ( module Website.Database.Models
  , module Website.Database.Pool
  , runMigration
  , runBeamDb
  ) where

import Control.Exception
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Lens.Micro
import Lens.Micro.Extras

import Website.Config
import Website.Database.Models
import Website.Database.Pool

import Paths_purefunctor_me


runMigration :: MonadIO m => Environment -> m ()
runMigration
  = liftIO
  . flip withConnPool runMigration_
  . view pool
  where
    runMigration_ conn = do
      defaultMigration <- getDataFileName "migration.sql"

      qs <- TIO.readFile "migration.sql" `catch`
        \(SomeException _) -> TIO.readFile defaultMigration

      forM_ ( filter (/= "") $ T.strip <$> T.splitOn ";" qs ) $
        execute_ conn . Query . (<> ";")


runBeamDb :: MonadIO m => Environment -> SqliteM a -> m a
runBeamDb env
  = liftIO
  . withConnPool (env^.pool)
  . flip runBeamSqlite
