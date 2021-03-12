module Website.Types where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Text ( Text, pack )

import Network.HTTP.Req as Req

import Servant ( Handler )

import Website.Config ( Environment )


type RequestM = ExceptT Text Req


instance MonadHttp RequestM where
  handleHttpException e = throwError (pack $ show e)


runRequestM :: RequestM r -> IO (Either Text r)
runRequestM = runReq defaultHttpConfig . runExceptT


type WebsiteM = ReaderT Environment Handler


runWebsiteM :: Environment -> WebsiteM a -> Handler a
runWebsiteM = flip runReaderT
