{-# LANGUAGE OverloadedStrings #-}
module Test.Utils where

import Test.Hspec
import Test.Hspec.Wai

import Control.Monad.IO.Class ( MonadIO )

import Data.Aeson ( FromJSON, Value, decode, encode, object, (.=) )

import qualified Data.List as List

import Data.ByteString ( ByteString )

import Network.HTTP.Types ( Header, methodDelete, methodPost, methodPut )
import Network.Wai.Test ( SResponse(simpleHeaders) )

import Web.Cookie ( SetCookie(setCookieName, setCookieValue), parseSetCookie )

import Website.Config


postJSON :: ByteString -> [Header] -> Value -> WaiSession st SResponse
postJSON path headers = request methodPost path headers' . encode
  where
    headers' = ("Content-Type", "application/json") : headers


putJSON :: ByteString -> [Header] -> Value -> WaiSession st SResponse
putJSON path headers = request methodPut path headers' . encode
  where
    headers' = ("Content-Type", "application/json") : headers


withAuth :: Configuration -> ([Header] -> WaiSession st a) -> WaiSession st a
withAuth config operations = do
  let loginPayload = object
        [ "username" .= adminUser config
        , "password" .= adminUser config
        ]

  mAuthHeaders <-
    mkAuthHeaders . parseSetCookies <$> postJSON "/login" [] loginPayload

  case mAuthHeaders of
    Just authHeaders -> operations authHeaders
    Nothing          -> fail "failed to create authentication headers"

  return undefined


delete' :: ByteString -> [Header] -> WaiSession st SResponse
delete' path headers = request methodDelete path headers ""


shouldContain' :: (MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldContain' lhs rhs = liftIO $ lhs `shouldContain` rhs


shouldBe' :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe' lhs rhs = liftIO $ lhs `shouldBe` rhs


parseSetCookies :: SResponse -> [SetCookie]
parseSetCookies
  = fmap (parseSetCookie . snd)
  . filter ((== "Set-Cookie") . fst)
  . simpleHeaders


mkAuthHeaders :: [SetCookie] -> Maybe [Header]
mkAuthHeaders setCookies = do
  jwtToken <- findCookieValue "JWT-Cookie"
  xsrfToken <- findCookieValue "XSRF-TOKEN"

  let jwtToken' = "JWT-Cookie=" <> jwtToken
  let xsrfToken' = "XSRF-TOKEN=" <> xsrfToken

  return [("Cookie", jwtToken' <> "; " <> xsrfToken'), ("X-XSRF-TOKEN", xsrfToken)]
  where
    findCookieValue name =
      setCookieValue <$> List.find ((== name) . setCookieName) setCookies


matchCodeJSON :: (Eq value, FromJSON value) => ResponseMatcher -> value -> ResponseMatcher
matchCodeJSON code value = code { matchBody = matchBody' }
  where
    matchBody' = MatchBody $ \_ body ->
      case decode body  of
        Just value' -> if value == value' then Nothing else Just "no match"
        Nothing     -> Just "no match"
