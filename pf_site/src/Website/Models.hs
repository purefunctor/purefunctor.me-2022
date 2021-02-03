module Website.Models where

import Data.Text ( Text )
import Data.Time ( UTCTime )

import qualified Database.Persist.TH as PTH


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  BlogPost json sql=post
    fullTitle Text
    shortTitle Text
    contents Text
    published UTCTime
    updated UTCTime
    Primary shortTitle
    deriving Eq Show
  Repository json sql=repo
    name Text
    owner Text
    url Text
    stars Int
    commits Int
    Primary name
    deriving Eq Show
|]
