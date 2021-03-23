{-# LANGUAGE TypeApplications #-}
module Website.Server.API.Blog where

import Control.Applicative
import Control.Lens
import Control.Monad ( unless )
import Control.Monad.Reader ( ask )

import           Data.Maybe ( isJust )
import           Data.String ( IsString(fromString) )
import           Data.Text ( Text, unpack )
import qualified Data.Text as Text
import           Data.Time ( UTCTime, getCurrentTime )

import Database.Beam

import Servant
import Servant.Auth.Server

import Website.Database
import Website.Server.API.Auth
import Website.Server.API.Common
import Website.Types


type BlogPostAPI =
  "blog" :>

    ( Get '[JSON] [BlogPost] :<|>

      Capture "short-title" Text :> Get '[JSON] BlogPost :<|>

      RequiresAuth
        :> ReqBody '[JSON] MutableBlogPostData
          :> Post '[JSON] MutableEndpointResult :<|>

      RequiresAuth
        :> Capture "short-title" Text
          :> ReqBody '[JSON] MutableBlogPostData
            :> Put '[JSON] MutableEndpointResult :<|>

      RequiresAuth
        :> Capture "short-title" Text
          :> Delete '[JSON] MutableEndpointResult
    )


data MutableBlogPostData
  = MutableBlogPostData
      { _title     :: Maybe Text
      , _slug      :: Maybe Text
      , _contents  :: Maybe Text
      , _published :: Maybe UTCTime
      , _updated   :: Maybe UTCTime
      }

deriveJSON' ''MutableBlogPostData
makeLenses ''MutableBlogPostData


blogPostServer :: ServerT BlogPostAPI WebsiteM
blogPostServer = getPosts :<|> getPost :<|> createPost :<|> updatePost :<|> deletePost
  where
    getPosts :: WebsiteM [BlogPost]
    getPosts = do
      env <- ask

      runBeamDb env $ runSelectReturningList $
        select $ all_ (websiteDb^.posts)

    getPost :: Text -> WebsiteM BlogPost
    getPost slug_ = do
      env <- ask

      post <- runBeamDb env $ runSelectReturningOne $
        select $ filter_ (\p -> p^.pSlug ==. fromString (unpack slug_)) (all_ $ websiteDb^.posts)

      case post of
        Just post' -> pure post'
        Nothing    -> throwError err404

    createPost
      :: AuthResult LoginPayload
      -> MutableBlogPostData
      -> WebsiteM MutableEndpointResult
    createPost (Authenticated _) payload = do
      env <- ask

      now <- liftIO getCurrentTime

      let
        autoShort =
          Text.intercalate "-"
          . take 3
          . Text.words

        mPost =
          BlogPost
            <$> payload^.title
            <*> ( payload^.slug <|>
                  payload^.title <&> autoShort
                )
            <*> payload^.contents
            <*> ( payload^.published <|> pure now )
            <*> ( payload^.updated <|> pure now )

      case mPost of
        Nothing -> throwError err400
        Just post -> do
          let
            keySlug = BlogPostSlug $
              fromString $ unpack $ post^.pSlug

          inDb <- runBeamDb env $ runSelectReturningOne $
            lookup_ (websiteDb^.posts) keySlug

          case inDb of
            Just _ -> throwError err400
            Nothing ->
              runBeamDb env $ runInsert $
                insert (websiteDb^.posts) $ insertValues [post]

          pure $ MutableEndpointResult 200 "Post created."

    createPost _ _ = throwError err401

    updatePost
      :: AuthResult LoginPayload
      -> Text
      -> MutableBlogPostData
      -> WebsiteM MutableEndpointResult
    updatePost (Authenticated _) slug_ payload = do
      env <- ask

      now <- liftIO getCurrentTime

      let
        keySlug = BlogPostSlug $ fromString $ unpack slug_

      mPost <- runBeamDb env $ runSelectReturningOne $
        lookup_ (websiteDb^.posts) keySlug

      unless
        ( any isJust
          [ payload^.title
          , payload^.contents
          , payload^.slug
          ]
        )
        $ throwError err400

      case mPost of
        Nothing -> throwError err400
        Just post ->
          runBeamDb env $ runUpdate $
            save (websiteDb^.posts)
              ( post & pTitle .~ payload^.title    . non (post^.pTitle)
                     & pCont  .~ payload^.contents . non (post^.pCont)
                     & pSlug  .~ payload^.slug     . non (post^.pSlug)
                     & pUpdt  .~ payload^.updated  . non now
              )

      pure $ MutableEndpointResult 200 "Post updated."

    updatePost _ _ _ = throwError err401

    deletePost
      :: AuthResult LoginPayload
      -> Text
      -> WebsiteM MutableEndpointResult
    deletePost (Authenticated _) slug_ = do
      env <- ask

      mPost <- runBeamDb env $ runSelectReturningOne $
        lookup_ (websiteDb^.posts) (BlogPostSlug $ fromString (unpack slug_))

      case mPost of
        Nothing -> throwError err404
        Just _ -> runBeamDb env $ runDelete $
          delete (websiteDb^.posts) (\p -> p^.pSlug ==. fromString (unpack slug_))

      pure $ MutableEndpointResult 200 "Post deleted."

    deletePost _ _ = throwError err401
