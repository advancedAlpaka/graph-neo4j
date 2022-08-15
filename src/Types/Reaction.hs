{-# LANGUAGE TemplateHaskell #-}

module Types.Reaction where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Database.Bolt.Extras.Template (makeNodeLike)
import GHC.Generics (Generic)

data Reaction = Reaction
  { id :: Int
  , name :: Text
  }
  deriving stock (Show, Generic)

makeLenses ''Reaction
makeNodeLike ''Reaction
