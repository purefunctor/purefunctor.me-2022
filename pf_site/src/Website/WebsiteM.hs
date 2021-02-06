module Website.WebsiteM where

import Control.Monad.Reader ( ReaderT, runReaderT )

import Servant ( Handler )

import Website.Config ( Environment )


type WebsiteM = ReaderT Environment Handler


runWebsiteM :: Environment -> WebsiteM a -> Handler a
runWebsiteM = flip runReaderT
