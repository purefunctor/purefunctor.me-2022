{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API.Repo where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Servant
import Website.Config
import Website.Models


type RepositoryAPI =
  "repo" :> Get '[JSON] [Repository] :<|>
  "repo" :> Capture "name" Text :> Get '[JSON] Repository :<|>
  "repo" :> ReqBody '[JSON] CreateRepositoryData:> Post '[JSON] Repository


data CreateRepositoryData = CreateRepositoryData
  { name  :: Text
  , owner :: Text
  } deriving (Generic, FromJSON, ToJSON)


repositoryServer :: ServerT RepositoryAPI WebsiteM
repositoryServer = getRepositories :<|> getRepository :<|> mkRepository
  where
    getRepositories :: WebsiteM [Repository]
    getRepositories = do
      (Configuration _ pool) <- ask
      
      repositories <- liftIO $ flip runSqlPersistMPool pool $
        selectList [ ] [ ]
        
      return $ entityVal <$> repositories

    getRepository :: Text -> WebsiteM Repository
    getRepository n = do
      (Configuration _ pool) <- ask
      
      repository <- liftIO $ flip runSqlPersistMPool pool $
        selectFirst [ RepositoryName ==. n ] [ ]
        
      case repository of
        (Just repository') -> return $ entityVal repository'
        Nothing -> throwError err404

    mkRepository :: CreateRepositoryData -> WebsiteM Repository
    mkRepository (CreateRepositoryData name owner) = do
      (Configuration _ pool) <- ask
      
      let url  = Text.concat [ "https://github.com/" , owner , "/" , name ]
      let repo = Repository name owner url (-1) (-1)
      
      void $ liftIO $ flip runSqlPersistMPool pool $ insert repo

      return repo
