{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Website.Models
  ( BlogPost(..)
  , Repository(..)
  )
  where


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
  Repository json sql=repo
    name Text
    owner Text
    url Text
    stars Int
    commits Int
|]
