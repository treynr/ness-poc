
-- | file: TestFile.hs 
-- | desc: Unit testing for functions in the File module.
-- | auth: TR
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestEntity (test_Entity) where

import Data.List             (nub, sort)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

import Entity
import Entity.Internal
import File

-- | Function shorthands
--
bg = buildGeneEntity
bgs = buildGeneSetEntity
bt = buildTermEntity

test_Entity = testGroup "Entity.hs unit tests" tests

tests = [ test_removeDuplicates
        , test_flattenEntities
        , test_flattenEntities2
        , test_flattenEntities'
        , test_tagEntities
        , test_tagEntities'
        , test_tagEntities''
        ]

test_removeDuplicates = testProperty "removeDuplicates property test" $
    \xs -> (sort $ removeDuplicates (xs :: [Int])) == (sort $ nub xs)

test_flattenEntities = testCase "flattenEntities test #1" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 

    (flattenEntities el V.empty) @?= v
    where
        v = V.fromList [ bg 10, bg 11, bg 12, bg 13, bg 14, bg 15, bg 16 ]

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

