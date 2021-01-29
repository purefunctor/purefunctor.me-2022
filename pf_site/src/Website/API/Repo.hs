{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Website.API.Repo where

import           Control.Monad ( void )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Reader
import           Data.Aeson ( FromJSON, ToJSON )
import           Data.Maybe ( fromMaybe )
import           Data.Text ( Text )
import qualified Data.Text as Text
import           Data.Time ( getCurrentTime )
import           Database.Persist.Sqlite
import           GHC.Generics ( Generic )
import           Servant
import           Servant.Auth
import           Servant.Auth.Server
import           Website.API.Auth
import           Website.Config
import           Website.Models
import           Website.WebsiteM


type RepositoryAPI =
  "repo" :>

    ( Get '[JSON] [Repository] :<|>

      Capture "name" Text :> Get '[JSON] Repository :<|>

      ( Auth '[JWT, Cookie] LoginPayload :>
        ( ReqBody '[JSON] CreateRepositoryData :> Post '[JSON] Repository
        )
      )

    )


data CreateRepositoryData
  = CreateRepositoryData
      { name  :: Text
      , owner :: Text
      }
  deriving (FromJSON, Generic, ToJSON)


repositoryServer :: ServerT RepositoryAPI WebsiteM
repositoryServer = getRepositories :<|> getRepository :<|> mkRepository
  where
    getRepositories :: WebsiteM [Repository]
    getRepositories = do
      pool <- asks connPool

      repositories <- liftIO $ flip runSqlPersistMPool pool $
        selectList [ ] [ ]

      return $ entityVal <$> repositories

    getRepository :: Text -> WebsiteM Repository
    getRepository n = do
      pool <- asks connPool

      repository <- liftIO $ flip runSqlPersistMPool pool $
        selectFirst [ RepositoryName ==. n ] [ ]

      case repository of
        (Just repository') -> return $ entityVal repository'
        Nothing            -> throwError err404

    mkRepository :: AuthResult LoginPayload -> CreateRepositoryData -> WebsiteM Repository
    mkRepository (Authenticated login) (CreateRepositoryData name owner) = do
      pool <- asks connPool

      let url  = Text.concat [ "https://github.com/" , owner , "/" , name ]
      let repo = Repository name owner url (-1) (-1)

      void $ liftIO $ flip runSqlPersistMPool pool $ insert repo

      return repo

    mkRepository _ _ = throwError err401
