module Website.Store where

import Data.Function (const)
import Data.Void (Void)
import Routing.PushState (PushStateInterface)

type Store =
  { pushInterface ∷ PushStateInterface
  }

type Action = Void

reduce ∷ Store → Action → Store
reduce store = const store
