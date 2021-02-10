module Website.Tasks where

import Control.Concurrent ( ThreadId )
import Control.Concurrent.Async

import Control.Lens

import Control.Monad
import Control.Monad.Except

import Data.Aeson
import Data.Aeson.Types

import Data.Text
import Data.Text.Encoding

import Database.Persist.Sqlite

import Network.HTTP.Req as Req

import System.Cron

import Website.Config
import Website.Models


type RequestM = ExceptT Text Req


instance MonadHttp RequestM where
  handleHttpException _ = throwError "an http exception was encountered"


runRequestM :: RequestM r -> IO (Either Text r)
runRequestM = runReq defaultHttpConfig . runExceptT


getRepositoryStats :: Environment -> Repository -> IO (Maybe (Int, Int))
getRepositoryStats env repository =
  do stars <- getStars
     commits <- getCommits
     return $ (,) <$> stars <*> commits
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

      return $ responseBody <$> response

    getStars :: IO (Maybe Int)
    getStars = do
      value <- getResource repoUrl
      case value of
        Left _ -> return Nothing
        Right val ->
          return $ parseMaybe (withObject "stargazers" (.: "stargazers_count")) val

    getCommits :: IO (Maybe Int)
    getCommits = do
      value <- getResource $ repoUrl /: "stats" /: "participation"
      case value of
        Left _ -> return Nothing
        Right val ->
          return $ sum <$>
            (parseMaybe (withObject "participation" (.: "all")) val :: Maybe [Int])


updateRepositoryStats :: Environment -> IO ()
updateRepositoryStats env = do
  repositories <-
    liftIO $ flip runSqlPersistMPool (env^.pool) $ selectList [ ] [ ]

  -- Pure operations are guaranteed to be safe
  repoStats <-
    forConcurrently (entityVal <$> repositories) $ \repository ->
      (,) repository <$> getRepositoryStats env repository

  -- Mindfulness of impure operations is a must
  forM_ repoStats $ \(repository, mStats) ->
    case mStats of
      Just (stars, commits) ->
        liftIO $ flip runSqlPersistMPool (env^.pool) $
          update (RepositoryKey $ repositoryName repository)
            [ RepositoryStars   =. stars
            , RepositoryCommits =. commits
            ]
      Nothing -> print "not updating repository"


runTasks :: Environment -> IO [ThreadId]
runTasks env = execSchedule $ addJob (updateRepositoryStats env) "0 */3 * * *"
