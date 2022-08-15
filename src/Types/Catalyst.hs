{-# LANGUAGE TemplateHaskell #-}

module Types.Catalyst where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Database.Bolt.Extras.Template (makeNodeLike)
import GHC.Generics (Generic)

data Catalyst = Catalyst
  { id :: Int
  , smiles :: Text
  , name :: Maybe Text
  }
  deriving stock (Show, Generic)

makeLenses ''Catalyst
makeNodeLike ''Catalyst