module Website.Database.Pool where

import Control.Exception

import Data.Pool

import Database.SQLite.Simple


type ConnPool = Pool Connection


mkConnPool :: FilePath -> Int -> IO ConnPool
mkConnPool database = createPool create destroy 1 600
  where
    create :: IO Connection
    create = open database

    destroy :: Connection -> IO ()
    destroy connection =
      close connection `catch`
        (\(SomeException _) -> putStrLn "Could not close the database.")


withConnPool :: ConnPool -> (Connection -> IO a) -> IO a
withConnPool = withResource
