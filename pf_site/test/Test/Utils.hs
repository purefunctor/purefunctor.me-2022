{-# LANGUAGE OverloadedStrings #-}
module Test.Utils where

import Test.Hspec
import Test.Hspec.Wai

import Control.Monad.IO.Class ( MonadIO )

import Data.Aeson.Micro ( Value, encode )

import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as LazyBS

import Text.Printf

import Network.HTTP.Types ( Header, methodPost )
import Network.Wai.Test ( SResponse(simpleHeaders) )

import Web.Cookie ( SetCookie(setCookieName), parseSetCookie )


postJSON :: ByteString -> [Header] -> Value -> WaiSession st SResponse
postJSON path headers = request methodPost path headers . encode


shouldContain' :: (MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldContain' lhs rhs = liftIO $ lhs `shouldContain` rhs


parseSetCookies :: SResponse -> [SetCookie]
parseSetCookies
  = fmap (parseSetCookie . snd)
  . filter ((== "Set-Cookie") . fst)
  . simpleHeaders
