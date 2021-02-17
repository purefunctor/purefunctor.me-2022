module PF.Data.Resources where


type BlogPost =
  { fullTitle :: String
  , shortTitle :: String
  , contents :: String
  , published :: String
  , updated :: String
  }


type Repository =
  { name :: String
  , owner :: String
  , url :: String
  , stars :: Int
  , commits :: Int
  }
