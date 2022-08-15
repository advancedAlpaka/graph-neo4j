{-# LANGUAGE TemplateHaskell #-}

module Types.ReactionDetail where

import Control.Lens (makeLenses)
import Data.Set (Set)
import Types.Catalyst qualified as C (Catalyst)
import Types.Molecule qualified as M (Molecule)
import Types.Reaction qualified as R (Reaction)
import Types.Relationships (ACCELERATE, PRODUCT_FROM, REAGENT_IN)

data ReactionDetail = ReactionDetail
  { info :: R.Reaction
  , reactives :: Set (REAGENT_IN, M.Molecule)
  , catalyst :: (ACCELERATE, C.Catalyst)
  , products :: Set (PRODUCT_FROM, M.Molecule)
  --  , fstReactive :: (REAGENT_IN, M.Molecule)
  --  , sndReactive :: (REAGENT_IN, M.Molecule)
  --  , product :: (PRODUCT_FROM, M.Molecule)
  }
  deriving stock (Show)

makeLenses ''ReactionDetail