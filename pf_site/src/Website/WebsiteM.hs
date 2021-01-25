module Website.WebsiteM where


import Control.Monad.Reader (ReaderT, runReaderT)
import Website.Config (Configuration)
import Servant (Handler)


type WebsiteM = ReaderT Configuration Handler


runWebsiteM :: Configuration -> WebsiteM a ->  Handler a
runWebsiteM = flip runReaderT
