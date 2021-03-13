module Website.Server.PseudoSSR.Static where

import Control.Monad.IO.Class ( MonadIO(liftIO) )

import qualified Data.List as List
import           Data.Text ( Text )

import Network.HTTP.Req

import qualified Text.HTML.TagSoup as TS

import Website.Server.PseudoSSR.Types
import Website.Types


getIndexFile :: MonadIO m => [Text] -> Int -> m (Either Text TagSoupHTML)
getIndexFile url_ port_ = liftIO $ runRequestM $
  TagSoupHTML . TS.parseTags . responseBody <$> req
    GET
    (List.foldl' (/:) (http $ head url_) (tail url_))
    NoReqBody
    lbsResponse
    (port port_)
