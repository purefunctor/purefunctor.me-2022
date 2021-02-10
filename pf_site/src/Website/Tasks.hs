module Website.Tasks where

import Control.Concurrent.Async

import Control.Lens

import Control.Monad
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.Aeson
import Data.Aeson.Types

import Data.Text.Encoding

import Database.Persist.Sqlite

import Network.HTTP.Req as Req

import Website.Config
import Website.Models


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

    getResource :: Url 'Https -> IO Value
    getResource url = fmap responseBody . runReq defaultHttpConfig $
      req GET url NoReqBody jsonResponse
        ( header "User-Agent" "purefunctor.me" <>
          basicAuth (encodeUtf8 $ env^.config.github.username)
                    (encodeUtf8 $ env^.config.github.token)
        )

    getStars :: IO (Maybe Int)
    getStars = do
      value <- getResource repoUrl
      return $ parseMaybe (withObject "stargazers" (.: "stargazers_count")) value

    getCommits :: IO (Maybe Int)
    getCommits = do
      value <- getResource $ repoUrl /: "stats" /: "participation"
      return $ sum <$>
        (parseMaybe (withObject "participation" (.: "all")) value :: Maybe [Int])


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
