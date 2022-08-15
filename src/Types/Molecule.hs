{-# LANGUAGE TemplateHaskell #-}

module Types.Molecule where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Database.Bolt.Extras.Template (makeNodeLike)
import GHC.Generics (Generic)

data Molecule = Molecule
  { id :: Int
  , smiles :: Text
  , iupacName :: Text
  }
  deriving stock (Show, Generic, Eq, Ord)

makeLenses ''Molecule
makeNodeLike ''Molecule
