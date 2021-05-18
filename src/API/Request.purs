module Website.API.Request where

import Prelude

import Affjax (Request, Response, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), hush)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Routing.Duplex (print)
import Website.API.Endpoint (Endpoint, endpointCodec)
import Website.Utils.Cookies (XsrfToken(..), getXsrfToken)


data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete


type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }


defaultRequest :: Maybe XsrfToken -> RequestOptions -> Request Json
defaultRequest mXsrfToken { endpoint, method } =
  { url: print endpointCodec endpoint
  , method: Left method'
  , content: RB.json <$> body
  , headers:
      case mXsrfToken of
        Nothing -> []
        Just (XsrfToken xsrfToken) ->
          [ RequestHeader "X-XSRF-TOKEN" xsrfToken ]
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , timeout: Nothing
  , responseFormat: RF.json
  }
  where
    Tuple method' body = case method of
      Get -> Tuple GET Nothing
      Post r -> Tuple POST r
      Put r -> Tuple PUT r
      Delete -> Tuple DELETE Nothing


mkRequest_
  :: forall m.
     MonadAff m
  => Maybe XsrfToken
  -> RequestOptions
  -> m (Maybe (Response Json))
mkRequest_ mXsrfToken options = do
  liftAff $ request (defaultRequest mXsrfToken options) >>= (pure <<< hush)


mkRequest :: forall m. MonadAff m => RequestOptions -> m (Maybe Json)
mkRequest options = do
  response <- mkRequest_ Nothing options
  pure $ _.body <$> response


mkAuthRequest :: forall m. MonadAff m => RequestOptions -> m (Maybe Json)
mkAuthRequest options = do
  case getXsrfToken of
    Left err -> log (show err) *> pure Nothing
    Right xsrf -> do
      response <- mkRequest_ (Just xsrf) options
      pure $ _.body <$> response
