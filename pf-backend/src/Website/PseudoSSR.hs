module Website.PseudoSSR
  ( module Website.PseudoSSR.Inject
  , module Website.PseudoSSR.Static
  , module Website.PseudoSSR.Types
  , PseudoSSR
  , ssrServer
  ) where

import Control.Lens

import Control.Monad.Trans.Reader ( asks )

import Servant

import Website.Config
import Website.PseudoSSR.Inject
import Website.PseudoSSR.Static
import Website.PseudoSSR.Types
import Website.Types


type PseudoSSR =
  Get '[HTML] TagSoupHTML :<|> "admin" :> Get '[HTML] TagSoupHTML


ssrServer :: ServerT PseudoSSR WebsiteM
ssrServer = getIndex :<|> getAdmin
  where
    getIndex_ :: WebsiteM TagSoupHTML
    getIndex_ = do
      ssr_ <- asks (view $ config.ssr)

      eIndexFile <-
        getIndexFile (ssr_^.staticUrl) (ssr_^.staticPort)

      case eIndexFile of
        Left _          -> throwError err404
        Right indexFile -> return indexFile

    tags :: Tags
    tags = Tags
      "Pure's Website"
      "https://avatars.githubusercontent.com/u/66708316"
      "purefunctor.me"

    getIndex :: WebsiteM TagSoupHTML
    getIndex = injectTags tags <$> getIndex_

    getAdmin :: WebsiteM TagSoupHTML
    getAdmin = injectTags tags { ogTitle = "Admin Page" } <$> getIndex_
