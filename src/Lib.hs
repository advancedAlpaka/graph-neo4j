module Lib
  ( addReaction,
    getReaction,
  findShortestPath)
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.State (execState, forM_, modify)
import Data.Maybe (fromMaybe)
import Data.Set (fromList)
import Data.Text (Text)
import Database.Bolt hiding ((=:))
import Database.Bolt.Extras (NodeLike (..), URelationLike (..))
import Database.Bolt.Extras.DSL (Selector (..), formQuery, matchF, returnF, textF)
import Database.Bolt.Extras.DSL.Typed (lbl, prop, (!-:), (-:), (.&), (=:), p)
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

getReaction :: Pipe -> Int -> IO (Either BoltError RD.ReactionDetail)
getReaction db recid = runE db $ do
  allRes <- query text
  let res = head allRes
  reactInfo :: R.Reaction <- extr fromNode res "reactInfo"
  cat :: C.Catalyst <- extr fromNode res "catalyst"
  molIns :: [M.Molecule] <- extr (map fromNode) res "reactives"
  molPrs :: [M.Molecule] <- extr (map fromNode) res "products"

  catInfo :: ACCELERATE <- extr (fromURelation . toU) res "catalystInfo"
  rsInfo :: [REAGENT_IN] <- extr (map $ fromURelation . toU) res "reactiveInfos"
  resInfo :: [PRODUCT_FROM] <- extr (map $ fromURelation . toU) res "productInfos"
  return $ RD.ReactionDetail reactInfo (fromList $ zip rsInfo molIns) (catInfo, cat) (fromList $ zip resInfo molPrs)
  where
    toU :: Relationship -> URelationship
    toU Relationship {relIdentity = _relIdentity, startNodeId = _startNodeId, endNodeId = _endNodeId, relType = _relType, relProps = _relProps} =
      URelationship {urelIdentity = _relIdentity, urelType = _relType, urelProps = _relProps}
    extr :: (RecordValue a) => (a -> b) -> Record -> Text -> BoltActionT IO b
    extr toB record key = do
      ans <- record `at` key
      --liftIO $ print ans
      case exactEither ans of
        Left ue -> throwError $ WrongMessageFormat ue
        Right a -> return $ toB a
    text = formQuery $ do
      matchF
        [ PS $ #cat .& lbl @C.Catalyst -: #catInfo .& lbl @ACCELERATE !-: #react .& lbl @R.Reaction .& prop (#id =: recid)
        ]
      returnF
        [ "react AS reactInfo",
          "catInfo AS catalystInfo",
          "cat AS catalyst",
          "[(react)-[prInfo:PRODUCT_FROM]-(pr:Molecule) | prInfo] AS productInfos",
          "[(react)-[prInfo:PRODUCT_FROM]-(pr:Molecule) | pr] AS products",
          "[(r:Molecule)-[rInfo:REAGENT_IN]-(react) | rInfo] AS reactiveInfos",
          "[(r:Molecule)-[rInfo:REAGENT_IN]-(react) | r] AS reactives"
        ]

{- Graph API Version
getReactionRequest :: Int -> GraphGetRequest
getReactionRequest id =
  flip execState emptyGraph $ do
    modify $ addNode "reactInfo" reactInfo
    modify $ addNode "catalyst" $ genNode ''C.Catalyst
    modify $ addNode "molIn1" mol
    modify $ addNode "molIn2" mol
    modify $ addNode "molRes" mol

    modify $ addRelation "molIn1" "reactInfo" $ genRel ''REAGENT_IN
    modify $ addRelation "molIn2" "reactInfo" $ genRel ''REAGENT_IN
    modify $ addRelation "catalyst" "reactInfo" $ genRel ''ACCELERATE
    modify $ addRelation "reactInfo" "molRes" $ genRel ''PRODUCT_FROM
  where
    mol = genNode ''M.Molecule
    reactInfo =
      genNode ''R.Reaction
        # withProp ("id", I id)
    genNode :: Name -> NodeGetter
    genNode name =
      defaultNodeReturn
        # withLabelQ name
        # withReturn allProps
    genRel :: Name -> RelGetter
    genRel name =
      defaultRelReturn
        # withLabelQ name
        # withReturn allProps

getReaction :: Pipe -> Int -> IO (Either BoltError RD.ReactionDetail)
getReaction db id = runE db $ do
  allRes <- makeRequest @GetRequest [] (getReactionRequest id)
  let res = head allRes
  let reactInfo :: R.Reaction = extractNode "reactInfo" res
  let cat :: C.Catalyst = extractNode "catalyst" res
  let molIn1 :: M.Molecule = extractNode "molIn1" res
  let molIn2 :: M.Molecule = extractNode "molIn2" res
  let molRes :: M.Molecule = extractNode "molRes" res

  let r1Info :: REAGENT_IN = extractRelation "molIn1" "reactInfo" res
  let r2Info :: REAGENT_IN = extractRelation "molIn2" "reactInfo" res
  let catInfo :: ACCELERATE = extractRelation "catalyst" "reactInfo" res
  let resInfo :: PRODUCT_FROM = extractRelation "reactInfo" "molRes" res
  return $ RD.ReactionDetail reactInfo (r1Info, molIn1) (r2Info, molIn2) (catInfo, cat) (resInfo, molRes)
  -}

findShortestPath :: Pipe -> Int -> Int -> IO (Either BoltError (Maybe Path))
findShortestPath db idFrom idTo = runE db $ do
  resQuery <- query text
  case resQuery of
    [] -> return Nothing
    (r : _) -> r `maybeAt` "path"
 where
  text = formQuery $ do
    matchF
      [ PS $ p $ #from .& lbl @M.Molecule .& prop (#id =: idFrom)
      , PS $ p $ #to .& lbl @M.Molecule .& prop (#id =: idTo)
      ]
    textF ", path = shortestPath((from)-[*]->(to))"
    returnF ["path"]