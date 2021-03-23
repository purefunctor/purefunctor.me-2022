{-# LANGUAGE TypeApplications #-}
module Website.Server.API.Repo where

import Control.Applicative
import Control.Lens
import Control.Monad ( unless )
import Control.Monad.Reader ( ask )

import           Data.Maybe ( fromMaybe, isJust )
import           Data.String ( IsString(fromString) )
import           Data.Text ( Text, unpack )
import qualified Data.Text as Text

import Database.Beam

import GHC.Int ( Int32 )

import Servant
import Servant.Auth.Server

import Website.Database
import Website.Server.API.Auth
import Website.Server.API.Common
import Website.Tasks
import Website.Types


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
          :> Delete '[JSON] MutableEndpointResult
    )


data MutableRepositoryData
  = MutableRepositoryData
      { _name        :: Maybe Text
      , _owner       :: Maybe Text
      , _url         :: Maybe Text
      , _language    :: Maybe Text
      , _description :: Maybe Text
      , _stars       :: Maybe Int
      , _commits     :: Maybe Int
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
      env <- ask

      runBeamDb env $ runSelectReturningList $
        select $ all_ (websiteDb^.repos)

    getRepository :: Text -> WebsiteM Repository
    getRepository name_ = do
      env <- ask

      mRepo <- runBeamDb env $ runSelectReturningOne $
        select $ filter_ (\r -> r^.rName ==. fromString (unpack name_)) (all_ $ websiteDb^.repos)

      case mRepo of
        Just repo' -> pure repo'
        Nothing    -> throwError err404

    createRepository
      :: AuthResult LoginPayload
      -> MutableRepositoryData
      -> WebsiteM MutableEndpointResult
    createRepository (Authenticated _) payload = do
      env <- ask

      let
        autoUrl o n =
          Text.concat [ "https://github.com/" , o , "/" , n ]

        mRepo =
          Repository @Identity
            <$> payload^.name
            <*> payload^.owner
            <*> ( payload^.url <|>
                  autoUrl
                    <$> payload^.owner
                    <*> payload^.name
                )
            <*> Just ""
            <*> Just "No description provided."
            <*> Just 0
            <*> Just 0

      case mRepo of
        Nothing -> throwError err400
        Just repo -> do
          let
            keyName = RepositoryName $
              fromString $ unpack $ repo^.rName

          inDb <- runBeamDb env $ runSelectReturningOne $
            lookup_ (websiteDb^.repos) keyName

          case inDb of
            Just _ -> throwError err400
            Nothing -> do
              mStats <- liftIO $ getRepositoryData env repo

              let
                repo' = case mStats of
                  Nothing -> repo
                  Just (ghDescr, ghLang, ghStars, ghCommits) ->
                    let
                      descr' = fromMaybe ghDescr   $ payload^.description
                      lang'  = fromMaybe ghLang    $ payload^.language
                      stars' = fromMaybe ghStars   $ payload^.stars
                      comms' = fromMaybe ghCommits $ payload^.commits

                      toInt32 :: Int -> Int32
                      toInt32 = toEnum . fromEnum
                    in
                      repo & rDesc .~ descr'
                           & rLang .~ lang'
                           & rStar .~ toInt32 stars'
                           & rComm .~ toInt32 comms'

              runBeamDb env $ runInsert $
                insert (websiteDb^.repos) $ insertValues [repo']

          pure $ MutableEndpointResult 200 "Repository created."

    createRepository _ _ = throwError err401

    updateRepository
      :: AuthResult LoginPayload
      -> Text
      -> MutableRepositoryData
      -> WebsiteM MutableEndpointResult
    updateRepository (Authenticated _) name_ payload = do
      env <- ask

      let
        keyName = RepositoryName @Identity $ fromString $ unpack name_

      mRepo <- runBeamDb env $ runSelectReturningOne $
        lookup_ (websiteDb^.repos) keyName

      unless
        ( any id
          [ isJust $ payload^.name
          , isJust $ payload^.owner
          , isJust $ payload^.url
          , isJust $ payload^.language
          , isJust $ payload^.description
          , isJust $ payload^.stars
          , isJust $ payload^.commits
          ]
        )
        $ throwError err400

      case mRepo of
        Nothing -> throwError err400
        Just repo ->
          let
            toInt32 :: Int -> Int32
            toInt32 = toEnum . fromEnum

            comms' = toInt32 <$> payload^.commits
            stars' = toInt32 <$> payload^.stars
          in
            runBeamDb env $ runUpdate $
              save (websiteDb^.repos)
                ( repo
                    & rName  .~ payload^.name  . non (repo^.rName)
                    & rOwner .~ payload^.owner . non (repo^.rOwner)

                    & rLang .~ payload^.language    . non (repo^.rLang)
                    & rDesc .~ payload^.description . non (repo^.rDesc)

                    & rComm .~ fromMaybe (repo^.rComm) comms'
                    & rStar .~ fromMaybe (repo^.rStar) stars'
                )

      pure $ MutableEndpointResult 200 "Repository updated."

    updateRepository _ _ _ = throwError err401

    deleteRepository
      :: AuthResult LoginPayload
      -> Text
      -> WebsiteM MutableEndpointResult
    deleteRepository (Authenticated _) name_ = do
      env <- ask

      mRepo <- runBeamDb env $ runSelectReturningOne $
        lookup_ (websiteDb^.repos) (RepositoryName $ fromString $ unpack name_)

      case mRepo of
        Nothing -> throwError err404
        Just _ -> runBeamDb env $ runDelete $
          delete (websiteDb^.repos) (\r -> r^.rName ==. fromString (unpack name_))

      pure $ MutableEndpointResult 200 "Repository deleted."

    deleteRepository _ _ = throwError err401
