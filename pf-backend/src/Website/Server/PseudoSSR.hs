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
      "PureFunctor"
      "https://avatars.githubusercontent.com/u/66708316"
      "purefunctor.me"
      "Full-stack web project written in Haskell and PureScript." 

    getIndex :: WebsiteM TagSoupHTML
    getIndex = injectTags tags <$> getIndex_

    getAdmin :: WebsiteM TagSoupHTML
    getAdmin = injectTags tags { ogTitle = "PureFunctor | Admin Page" } <$> getIndex_
