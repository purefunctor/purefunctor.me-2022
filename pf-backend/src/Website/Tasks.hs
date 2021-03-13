module Website.Tasks where

import Control.Concurrent ( ThreadId )
import Control.Concurrent.Async

import Control.Lens

import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) )

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import Data.Text.Encoding

import Database.Persist.Sqlite

import Network.HTTP.Req as Req

import System.Cron

import Website.Config
import Website.Models
import Website.Types


getRepositoryData :: Environment -> Repository -> IO (Maybe (Text, Int, Int))
getRepositoryData env repository = runMaybeT $
  do (stars, descr) <- getGeneralData
     commits <- getCommits
     pure (descr, stars, commits)
  where
    repoUrl :: Url 'Https
    repoUrl =
      https "api.github.com"
      /: "repos"
      /: repositoryOwner repository
      /: repositoryName repository

    getResource :: Url 'Https -> IO (Either Text Value)
    getResource url = do
      response <- runRequestM $
        req GET url NoReqBody jsonResponse
            ( header "User-Agent" "purefunctor.me" <>
              basicAuth (encodeUtf8 $ env^.config.github.username)
                        (encodeUtf8 $ env^.config.github.token)
            )

      pure $ responseBody <$> response

    getGeneralData :: MaybeT IO (Int, Text)
    getGeneralData = do
      value <- liftIO $ getResource repoUrl
      case value of
        Left err ->
          liftIO (runStderrLoggingT $ logErrorN err) >> mzero
        Right val -> MaybeT . pure $
          (,) <$> parseMaybe (withObject "stargazers" (.: "stargazers_count")) val
              <*> parseMaybe (withObject "description" (.: "description")) val

    getCommits :: MaybeT IO Int
    getCommits = do
      value <- liftIO $ getResource $ repoUrl /: "stats" /: "participation"
      case value of
        Left err ->
          liftIO (runStderrLoggingT $ logErrorN err) >> mzero
        Right val -> MaybeT . pure $
          sum <$>
            (parseMaybe (withObject "participation" (.: "all")) val :: Maybe [Int])


updateRepositoryData :: Environment -> IO ()
updateRepositoryData env = do
  repositories <-
    liftIO $ flip runSqlPersistMPool (env^.pool) $ selectList [ ] [ ]

  -- Pure operations are guaranteed to be safe
  repoStats <-
    forConcurrently (entityVal <$> repositories) $ \repository ->
      (,) repository <$> getRepositoryData env repository

  -- Mindfulness of impure operations is a must
  forM_ repoStats $ \(repository, mStats) ->
    case mStats of
      Just (descr, stars, commits) ->
        liftIO $ flip runSqlPersistMPool (env^.pool) $
          update (RepositoryKey $ repositoryName repository)
            [ RepositoryStars       =. stars
            , RepositoryCommits     =. commits
            , RepositoryDescription =. descr
            ]
      Nothing -> runStderrLoggingT $ logErrorN "not updating repository"


runTasks :: Environment -> IO [ThreadId]
runTasks env = execSchedule $ addJob (updateRepositoryData env) "0 */3 * * *"
