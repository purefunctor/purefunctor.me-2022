module Website.Server.PseudoSSR
  ( module Website.Server.PseudoSSR.Inject
  , module Website.Server.PseudoSSR.Static
  , module Website.Server.PseudoSSR.Types
  , PseudoSSR
  , ssrServer
  ) where

import Control.Lens

import Control.Monad.Trans.Reader ( asks )

import Servant

import Website.Config
import Website.Server.PseudoSSR.Inject
import Website.Server.PseudoSSR.Static
import Website.Server.PseudoSSR.Types
import Website.Types


type PseudoSSR =
  Get '[HTML] TagSoupHTML :<|>

  "admin" :> Get '[HTML] TagSoupHTML :<|>

  "404.html" :> Verb 'GET 404 '[HTML] TagSoupHTML


ssrServer :: ServerT PseudoSSR WebsiteM
ssrServer = getIndex :<|> getAdmin :<|> get404
  where
    getIndex_ :: WebsiteM TagSoupHTML
    getIndex_ = do
      ssr_ <- asks (view $ config.ssr)

      eIndexFile <-
        getIndexFile (ssr_^.staticUrl) (ssr_^.staticPort)

      case eIndexFile of
        Left _          -> throwError err404
        Right indexFile -> return indexFile

    tags :: MetaTags
    tags = MetaTags
      "PureFunctor"
      "PureFunctor"
      "https://avatars.githubusercontent.com/u/66708316"
      "https://purefunctor.me/"
      "Full-stack web project written in Haskell and PureScript."

    getIndex :: WebsiteM TagSoupHTML
    getIndex = injectMeta tags <$> getIndex_

    getAdmin :: WebsiteM TagSoupHTML
    getAdmin = injectMeta tags
      { title = "PureFunctor | Admin Page"
      , ogTitle = "PureFunctor | Admin Page"
      , ogUrl = "https://purefunctor.me/admin"
      } <$> getIndex_


    get404 :: WebsiteM TagSoupHTML
    get404 = injectMeta tags
      { title = "404 Not Found"
      , ogTitle = "404 Not Found"
      , ogUrl = "https://purefunctor.me/404.html"
      , ogDesc = "404 Not Found"
      } <$> getIndex_
