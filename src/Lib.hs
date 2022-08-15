module Lib (addReaction) where

import Control.Monad.State (execState, forM_, modify)
import Data.Maybe (fromMaybe)
import Data.Set (fromList)
import Database.Bolt hiding ((=:))
import Database.Bolt.Extras (NodeLike (..), URelationLike (..))
import Database.Bolt.Extras.Graph qualified as G
import Types.Catalyst qualified as C (Catalyst (..))
import Types.Molecule qualified as M (Molecule (..))
import Types.Reaction qualified as R (Reaction (..))
import Types.ReactionDetail qualified as RD (ReactionDetail (..))
import Types.Relationships (ACCELERATE, PRODUCT_FROM, REAGENT_IN)

addReactionRequest :: RD.ReactionDetail -> G.GraphPutRequest
addReactionRequest (RD.ReactionDetail info rs (catRel, cat) prs) =
  let catName = "catalyst" <> fromMaybe (C.smiles cat) (C.name cat)
   in flip execState G.emptyGraph $ do
        modify $ G.addNode (R.name info) (G.MergeN $ toNode info)
        forM_
          rs
          ( \(rel, r) -> do
              modify $ G.addNode (M.iupacName r) (G.MergeN $ toNode r)
              modify $ G.addRelation (M.iupacName r) (R.name info) (G.MergeR $ toURelation rel)
          )
        modify $ G.addNode catName (G.MergeN $ toNode cat)
        modify $ G.addRelation catName (R.name info) (G.MergeR $ toURelation catRel)
        forM_
          prs
          ( \(prRel, pr) -> do
              modify $ G.addNode (M.iupacName pr) (G.MergeN $ toNode pr)
              modify $ G.addRelation (R.name info) (M.iupacName pr) (G.MergeR $ toURelation prRel)
          )

addReaction :: Pipe -> RD.ReactionDetail -> IO (Either BoltError [G.GraphPutResponse])
addReaction db react = runE db $ G.makeRequest @G.PutRequest [] $ addReactionRequest react
