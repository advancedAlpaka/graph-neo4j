module Main where

import Control.Monad (forM_)
import Data.Default (def)
import Data.List (unfoldr)
import Data.Set qualified as S (fromList, singleton)
import Data.Text (pack)
import Database.Bolt
import GHC.Base (build)
import Lib (findShortestPath)
import System.Random (mkStdGen, uniformR)
import Types.Catalyst qualified as C
import Types.Molecule qualified as M
import Types.Reaction qualified as R
import Types.ReactionDetail qualified as RD
import Types.Relationships (ACCELERATE (ACCELERATE), PRODUCT_FROM (PRODUCT_FROM), REAGENT_IN (REAGENT_IN))

boltCfg :: BoltCfg
boltCfg =
  def
    { host = "localhost"
    , user = "neo4j"
    , password = "12345"
    }

{-
  [ ("carbon", M.Molecule 1 "C" "carbon"),
    ("oxygen", M.Molecule 2 "O=O" "molecular oxygen"),
    ("dry ice", M.Molecule 3 "C(=O)=O" "carbon dioxide"),
    ("hydrogen", M.Molecule 4 "[HH]" "molecular hydrogen"),
    ("ethylene", M.Molecule 5 "C=C" "ethene"),
    ("ethane", M.Molecule 6 "CC" "ethane"),
    ("ethyne", M.Molecule 14 "C#C" "acetylene"),
    ("water", M.Molecule 7 "O" "oxidane"),
    ("copper", M.Molecule 8 "[Cu]" "copper"),
    ("cupric oxide", M.Molecule 9 "O=[Cu]" "oxocopper"),
    ("bromine", M.Molecule 10 "BrBr" "molecular bromine"),
    ("bromane", M.Molecule 15 "Br" "bromane"),
    ("ethylene bromide", M.Molecule 11 "C(CBr)Br" "1,2-dibromoethane"),
    ("ethanol", M.Molecule 12 "CCO" "ethanol"),
    ("chloroethene", M.Molecule 13 "C=CCl" "chloroethene"),
    ("iodane", M.Molecule 16 "I" "iodane"),
    ("phosphorus", M.Molecule 17 "P#P" "phosphanylidynephosphane")
  ]
-}

molecules :: [M.Molecule]
molecules = fmap (\mocId -> M.Molecule mocId "" ("mol" <> pack (show mocId))) [1 .. 40]

reactions :: [R.Reaction]
reactions = fmap (\recId -> R.Reaction recId ("react" <> pack (show recId))) [1 .. 40]

catalysts :: [C.Catalyst]
catalysts = fmap (\catId -> C.Catalyst catId ("cat" <> pack (show catId)) Nothing) [1 .. 20]

reactionsD :: [RD.ReactionDetail]
reactionsD =
  zipWith
    ( curry
        ( \([f1, f2, to, catId], r) ->
            RD.ReactionDetail
              { RD.info = r
              , RD.reactives = S.fromList [(REAGENT_IN 1.0, molecules !! f1), (REAGENT_IN 2.0, molecules !! f2)]
              , RD.catalyst = if catId > 19 then noCatalyst else (ACCELERATE 10.0 20.0, catalysts !! catId)
              , RD.products = S.singleton (PRODUCT_FROM 1.0, molecules !! to)
              }
        )
    )
    ids
    reactions
 where
  chunksOf :: Int -> [e] -> [[e]]
  chunksOf i ls = map (take i) (build (splitter ls))
   where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n
  ids :: [[Int]]
  ids = chunksOf 4 ((take 80 . unfoldr (Just . uniformR (0, 39))) $ mkStdGen 15645)

-- Data.List.find (\t ->  and $ fmap (\l -> size (fromList l) == 4) $ chunksOf 4 $ Prelude.take 80 . unfoldr (Just . uniformR (0 :: Int, 19)) $ mkStdGen t) [-1000..1000]

noCatalyst :: (ACCELERATE, C.Catalyst)
noCatalyst = (ACCELERATE 1.0 1.0, C.Catalyst 0 "" Nothing)

main :: IO ()
main = do
  db <- connect boltCfg
  -- forM_ (zip [1 .. 40] reactionsD) $ \(i, react) -> do
  --  print @Int i
  --  res <- addReaction db react
  --  print res
  -- res <- getReaction db 5
  -- print res
  a <- findShortestPath db 5 1
  close db
