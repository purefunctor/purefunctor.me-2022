module Website.WebsiteM where


import Control.Monad.Reader ( ReaderT, runReaderT )
import Servant ( Handler )
import Website.Config ( Configuration )


type WebsiteM = ReaderT Configuration Handler


runWebsiteM :: Configuration -> WebsiteM a ->  Handler a
runWebsiteM = flip runReaderT
