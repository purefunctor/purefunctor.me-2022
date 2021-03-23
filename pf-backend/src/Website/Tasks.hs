{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
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
import Data.List ( maximumBy )
import Data.Map ( Map, toList )
import Data.Text
import Data.Text.Encoding

import Database.Beam

import GHC.Int ( Int32 )

import Network.HTTP.Req as Req

import System.Cron

import Website.Config
import Website.Database
import Website.Types


getRepositoryData
  :: Environment
  -> Repository
  -> IO (Maybe (Text, Text, Int, Int))
getRepositoryData env repository = runMaybeT $
  do (stars, descr) <- getGeneralData
     commits <- getCommits
     language <- getLanguage
     pure (descr, language, stars, commits)
  where
    repoUrl :: Url 'Https
    repoUrl =
      https "api.github.com"
      /: "repos"
      /: view rOwner repository
      /: view rName repository

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

    getLanguage :: MaybeT IO Text
    getLanguage = do
      value <- liftIO $ getResource $ repoUrl /: "languages"

      languages <-
        case value of
          Left err ->
            liftIO (runStderrLoggingT $ logErrorN err) >> mzero
          Right val -> MaybeT . pure $
            toList <$> ( parseMaybe parseJSON val :: Maybe (Map Text Int) )

      pure $ fst $ maximumBy ( curry $ compare <$> fst <*> fst ) languages


updateRepositoryData :: Environment -> IO ()
updateRepositoryData env = do
  repositories <- runBeamDb env $ runSelectReturningList $
    select $ all_ (websiteDb^.repos)

  -- Pure operations are guaranteed to be safe
  repoStats <-
    forConcurrently repositories $ \repository ->
      (,) repository <$> getRepositoryData env repository

  -- Mindfulness of impure operations is a must
  forM_ repoStats $ \(repository, mStats) ->
    case mStats of
      Nothing ->
        runStderrLoggingT $ logErrorN "not updating repository"
      Just (descr, language, stars, comms) ->
        let
          toInt32 :: Int -> Int32
          toInt32 = toEnum . fromEnum
        in
          runBeamDb env $ runUpdate $
            save (websiteDb^.repos)
              ( repository & rDesc .~ descr
                           & rLang .~ language
                           & rStar .~ toInt32 stars
                           & rComm .~ toInt32 comms
              )


runTasks :: Environment -> IO [ThreadId]
runTasks env = execSchedule $ addJob (updateRepositoryData env) "0 */3 * * *"
