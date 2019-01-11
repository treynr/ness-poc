
-- | file: TestFile.hs 
-- | desc: Unit testing for functions in the File module.
-- | auth: TR
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestGraph (test_Graph) where

import Data.List        (sort)
import Data.Tuple       (swap)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Entity
import File
import Graph

-- | Function shorthands
--
bg = buildGeneEntity
bgs = buildGeneSetEntity
bt = buildTermEntity

test_Graph = testGroup "Graph.hs unit tests" tests

tests = [ test_updateAdjacencyList
        , test_updateAdjacencyList2
        , test_updateAdjacencyList'
        , test_updateAdjacencyList'2
        , test_buildDirectedGraph
        , test_buildDirectedGraph'
        , test_buildUndirectedGraph
        , test_buildUndirectedGraph'
        ]

test_updateAdjacencyList = testCase "updateAdjacencyList test #1" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 
    let flats = flattenEntities el V.empty

    (sort $ updateAdjacencyList False (tagEntities flats) el []) @?= (sort $ al)
    where
        al = [ ((0, 1), 1.0),  ((0, 2), 1.0), ((0, 3), 1.0), ((1, 4), 1.0)
             , ((1, 5), 1.0), ((1, 6), 1.0)
             ]

test_updateAdjacencyList2 = testCase "updateAdjacencyList test #2" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 
    let flats = flattenEntities el V.empty

    (sort $ updateAdjacencyList True (tagEntities flats) el []) @?= (sort $ (al ++ fmap (\(a, b) -> (swap a, b)) al))
    where
        al = [ ((0, 1), 1.0),  ((0, 2), 1.0), ((0, 3), 1.0), ((1, 4), 1.0)
             , ((1, 5), 1.0), ((1, 6), 1.0)
             ]

test_updateAdjacencyList' = testCase "updateAdjacencyList' test #1" $ do
    gs <- readGenesetFile "test/data/sample-genesets.tsv"
    let flats = flattenEntities' gs V.empty

    (sort $ updateAdjacencyList' False (tagEntities flats) gs []) @?= (sort $ al)
    where
        al = [ ((6, 4), 1.0),  ((6, 5), 1.0), ((7, 0), 1.0), ((7, 1), 1.0)
             , ((7, 2), 1.0), ((8, 3), 1.0)
             ]

test_updateAdjacencyList'2 = testCase "updateAdjacencyList' test #2" $ do
    gs <- readGenesetFile "test/data/sample-genesets.tsv"
    let flats = flattenEntities' gs V.empty

    (sort $ updateAdjacencyList' True (tagEntities flats) gs []) @?= (sort $ (al ++ fmap (\(a, b) -> (swap a, b)) al))
    where
        al = [ ((6, 4), 1.0),  ((6, 5), 1.0), ((7, 0), 1.0), ((7, 1), 1.0)
             , ((7, 2), 1.0), ((8, 3), 1.0)
             ]

test_buildDirectedGraph = testCase "buildDirectedGraph test" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 
    let flats = flattenEntities el V.empty

    buildDirectedGraph (tagEntities flats) el M.empty @?= g

    where
        g = M.fromList [ (0, S.fromList [1, 2, 3]), (1, S.fromList [4, 5, 6]) ]

test_buildDirectedGraph' = testCase "buildDirectedGraph' test" $ do
    gs <- readGenesetFile "test/data/sample-genesets.tsv"
    let flats = flattenEntities' gs V.empty

    buildDirectedGraph' (tagEntities flats) gs M.empty @?= g

    where
        g = M.fromList [ (6, S.fromList [4, 5]), (7, S.fromList [0, 1, 2])
                       , (8, S.fromList [3])
                       ]

test_buildUndirectedGraph = testCase "buildDirectedGraph test" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 
    let flats = flattenEntities el V.empty

    buildUndirectedGraph (tagEntities flats) el M.empty @?= g

    where
        g = M.fromList [ (0, S.fromList [1, 2, 3]), (1, S.fromList [4, 5, 6])
                       , (1, S.fromList [0, 4, 5, 6])
                       , (2, S.fromList [0]) 
                       , (3, S.fromList [0]), (4, S.fromList [1]) 
                       , (5, S.fromList [1]), (6, S.fromList [1]) 
                       ]

test_buildUndirectedGraph' = testCase "buildDirectedGraph' test" $ do
    gs <- readGenesetFile "test/data/sample-genesets.tsv"
    let flats = flattenEntities' gs V.empty

    buildUndirectedGraph' (tagEntities flats) gs M.empty @?= g

    where
        g = M.fromList [ (6, S.fromList [4, 5]), (7, S.fromList [0, 1, 2])
                       , (8, S.fromList [3])
                       , (4, S.fromList [6]), (5, S.fromList [6])
                       , (0, S.fromList [7]), (1, S.fromList [7])
                       , (2, S.fromList [7]), (3, S.fromList [8])
                       ]

{-
test_flattenEntities' = testCase "flattenEntities' test" $ do
    gs <- readGenesetFile "test/data/sample-genesets.tsv"

    (flattenEntities' gs V.empty) @?= v
    where
        v = V.fromList [ bg 10, bg 11, bg 12, bg 13, bg 15, bg 16, bgs 75605 9606
                       , bgs 246376 9606, bgs 246626 9606
                       ]

test_updateAdjacencyList' = testCase "updateAdjacencyList test #2" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 
    let flats = flattenEntities el V.empty

    (sort $ updateAdjacencyList True (tagEntities flats) el []) @?= (sort $ al)
    where
        al = [ ((0, 1), 1.0),  ((0, 2), 1.0), ((0, 3), 1.0), ((1, 4), 1.0)
             , ((1, 5), 1.0), ((1, 6), 1.0)
             ,  ((1, 0), 1.0),  ((2, 0), 1.0), ((3, 0), 1.0), ((4, 1), 1.0)
             , ((5, 1), 1.0), ((6, 1), 1.0)
             ]
-}
{-
test_flattenEntities2 = testCase "flattenEntities test #2" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 
    ont <- readTermFile "test/data/sample-ontology.tsv" 

    (flattenEntities ont $ flattenEntities el V.empty) @?= v
    where
        v = V.fromList [ bg 10, bg 11, bg 12, bg 13, bg 14, bg 15, bg 16 
                       , bt "GO:0007399" "", bt "GO:0021675" "", bt "GO:0022008" ""
                       , bt "GO:0031641" "", bt "GO:0048731" "", bt "GO:0051960" ""
                       ]

test_flattenEntities' = testCase "flattenEntities' test" $ do
    gs <- readGenesetFile "test/data/sample-genesets.tsv"

    (flattenEntities' gs V.empty) @?= v
    where
        v = V.fromList [ bg 10, bg 11, bg 12, bg 13, bg 15, bg 16, bgs 75605 9606
                       , bgs 246376 9606, bgs 246626 9606
                       ]

test_tagEntities = testCase "tagEntities test #1" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 

    (tagEntities $ flattenEntities el V.empty) @?= v
    where
        v = M.fromList [ (bg 10, 0), (bg 11, 1), (bg 12, 2), (bg 13, 3)
                       , (bg 14, 4), (bg 15, 5), (bg 16, 6)
                       ]

test_tagEntities' = testCase "tagEntities test #2" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 

    (tagEntities $ flattenEntities el $ flattenEntities el V.empty) @?= v
    where
        v = M.fromList [ (bg 10, 0), (bg 11, 1), (bg 12, 2), (bg 13, 3)
                       , (bg 14, 4), (bg 15, 5), (bg 16, 6)
                       ]

test_tagEntities'' = testCase "tagEntities test #3" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 
    ont <- readTermFile "test/data/sample-ontology.tsv" 

    (tagEntities $ flattenEntities ont $ flattenEntities el V.empty) @?= v
    where
        v = M.fromList [ (bg 10, 0), (bg 11, 1), (bg 12, 2), (bg 13, 3)
                       , (bg 14, 4), (bg 15, 5), (bg 16, 6)
                       , (bt "GO:0007399" "", 7), (bt "GO:0021675" "", 8)
                       , (bt "GO:0022008" "", 9), (bt "GO:0031641" "", 10)
                       , (bt "GO:0048731" "", 11), (bt "GO:0051960" "", 12)
                       ]
-}
