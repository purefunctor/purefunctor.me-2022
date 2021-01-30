module Website.API.Repo where

import Control.Applicative

import Control.Lens

import Control.Monad ( void )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Reader ( asks )

import Data.Maybe ( isJust )

import           Data.Text ( Text )
import qualified Data.Text as Text

import Database.Persist.Sqlite

import Servant
import Servant.Auth.Server

import Website.API.Auth
import Website.API.Common
import Website.Config
import Website.Models
import Website.Utils
import Website.WebsiteM


type RepositoryAPI =
  "repo" :>

    ( Get '[JSON] [Repository] :<|>

      Capture "name" Text :> Get '[JSON] Repository :<|>

      RequiresAuth
        :> ReqBody '[JSON] MutableRepositoryData
          :> Post '[JSON] MutableEndpointResult :<|>

      RequiresAuth
        :> Capture "name" Text
          :> ReqBody '[JSON] MutableRepositoryData
            :> Put '[JSON] MutableEndpointResult :<|>

      RequiresAuth
        :> Capture "name" Text
          :> Put '[JSON] MutableEndpointResult
    )


data MutableRepositoryData
  = MutableRepositoryData
      { _name    :: Maybe Text
      , _owner   :: Maybe Text
      , _url     :: Maybe Text
      , _stars   :: Maybe Int
      , _commits :: Maybe Int
      }

deriveJSON' ''MutableRepositoryData
makeLenses ''MutableRepositoryData


repositoryServer :: ServerT RepositoryAPI WebsiteM
repositoryServer =
  getRepositories :<|> getRepository :<|>
  createRepository :<|> updateRepository :<|> deleteRepository
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

    createRepository
      :: AuthResult LoginPayload
      -> MutableRepositoryData
      -> WebsiteM MutableEndpointResult
    createRepository (Authenticated _) payload = do
      pool <- asks connPool

      let autoUrl o n = Text.concat [ "https://github.com" , o , "/" , n ]

      let mRepo = Repository
            <$> payload ^. name
            <*> payload ^. owner
            <*> ( payload ^. url <|>
                  autoUrl
                    <$> payload ^. owner
                    <*> payload ^. name
                )
            <*> payload ^. stars
            <*> payload ^. commits

      case mRepo of

        Just repo -> do
          void $ liftIO $ flip runSqlPersistMPool pool $ insert repo

          let message = "Repository created: " <> repositoryName repo
          let result = MutableEndpointResult 200 message

          return result

        Nothing -> throwError err400

    createRepository _ _ = throwError err401

    updateRepository
      :: AuthResult LoginPayload
      -> Text
      -> MutableRepositoryData
      -> WebsiteM MutableEndpointResult
    updateRepository (Authenticated _) rName payload = do
      pool <- asks connPool

      let mUpdates = filter isJust
            [ (RepositoryName    =.) <$> payload ^. name
            , (RepositoryOwner   =.) <$> payload ^. owner
            , (RepositoryUrl     =.) <$> payload ^. url
            , (RepositoryStars   =.) <$> payload ^. stars
            , (RepositoryCommits =.) <$> payload ^. commits
            ]

      case mUpdates of

        [] -> throwError err400

        mUpdates' -> do
          case sequenceA mUpdates' of

            Just updates -> do
              void $ liftIO $ flip runSqlPersistMPool pool $
                update (RepositoryKey rName) updates
              return $ MutableEndpointResult 200 "Repository updated."

            Nothing -> throwError err400

    updateRepository _ _ _ = throwError err401

    deleteRepository
      :: AuthResult LoginPayload
      -> Text
      -> WebsiteM MutableEndpointResult
    deleteRepository (Authenticated _) rName = do
      pool <- asks connPool

      inDatabase <- liftIO $ flip runSqlPersistMPool pool $
        exists [ RepositoryName ==. rName ]

      if inDatabase
        then do
          liftIO $ flip runSqlPersistMPool pool $ delete $ RepositoryKey rName
          return $ MutableEndpointResult 200 "Repository deleted."
        else
          throwError err404

    deleteRepository _ _ = throwError err401
