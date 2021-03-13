module Website.Server.Extra
  ( custom404
  ) where

import Control.Lens ( (^.) )

import Network.HTTP.Types.Status
import Network.Wai ( responseLBS )

import Servant

import qualified Text.HTML.TagSoup as TS

import Website.Config
import Website.Server.PseudoSSR.Inject
import Website.Server.PseudoSSR.Static
import Website.Server.PseudoSSR.Types
import Website.Types



custom404 :: Environment -> ServerT Raw WebsiteM
custom404 env  = Tagged $ \_ callback -> do
  eIndexFile <- getIndexFile
    (env^.config.ssr.staticUrl)
    (env^.config.ssr.staticPort)

  let
    mkResponse =
      responseLBS
        status404
        [("Content-Type", "text/html")]

    tags :: MetaTags
    tags = MetaTags
      "404 Not Found"
      "404 Not Found"
      "https://avatars.githubusercontent.com/u/66708316"
      "https://purefunctor.me/404.html"
      "404 Not Found"

  callback . mkResponse . TS.renderTags . unTagSoupHTML . injectMeta tags $
    case eIndexFile of
      Left _ ->
        TagSoupHTML . TS.parseTags $
          "<!DOCTYPE html><html><head></head><body><h1>404 Page Not Found</h1></body></html>"
      Right indexFile ->
        indexFile
