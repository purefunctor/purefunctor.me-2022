module Website.PseudoSSR.Static where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.Text ( Text )

import qualified Data.List as List

import Network.HTTP.Req

import qualified Text.HTML.TagSoup as TS

import Website.PseudoSSR.Types
import Website.Types


getIndexFile :: MonadIO m => [Text] -> Int -> m (Either Text TagSoupHTML)
getIndexFile url_ port_ = liftIO $ runRequestM $
  TagSoupHTML . TS.parseTags . responseBody <$> req
    GET
    (List.foldl' (/:) (http $ head url_) (tail url_))
    NoReqBody
    lbsResponse
    (port port_)
