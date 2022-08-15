{-# LANGUAGE TemplateHaskell #-}

module Types.Relationships where

import Control.Lens (makeLenses)
import Database.Bolt.Extras.Template (makeURelationLike)
import GHC.Generics (Generic)

data ACCELERATE = ACCELERATE
  { temperature :: Float
  , pressure :: Float
  }
  deriving stock (Show, Generic)

makeLenses ''ACCELERATE
makeURelationLike ''ACCELERATE

newtype PRODUCT_FROM = PRODUCT_FROM {amount :: Float}
  deriving stock (Show, Generic, Eq, Ord)

makeLenses ''PRODUCT_FROM
makeURelationLike ''PRODUCT_FROM

newtype REAGENT_IN = REAGENT_IN {amount :: Float} 
  deriving stock (Show, Generic, Eq, Ord)

makeURelationLike ''REAGENT_IN
makeLenses ''PRODUCT_FROM