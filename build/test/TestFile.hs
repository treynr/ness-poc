
-- | file: TestFile.hs 
-- | desc: Unit testing for functions in the File module.
-- | auth: TR
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestFile (test_File) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector as V

import Entity
import File

-- | Function shorthands
--
bg = buildGeneEntity
bgs = buildGeneSetEntity
bt = buildTermEntity

test_File = testGroup "File.hs unit tests" tests

tests = [ test_parseEdgeListFile
        , test_readEdgeListFile
        , test_parseGenesetFile 
        , test_readGenesetFile
        , test_parseAnnotationFile
        , test_readAnnotationFile
        , test_parseTermFile
        , test_readTermFile
        ]

test_parseEdgeListFile = testCase "Edge list file parsing #1" $
    parseEdgeListFile "0\t1\t2\n0\t1\t3\n0\t2\t4" @?= v
    where
        v = V.fromList [(bg 1, bg 2), (bg 1, bg 3), (bg 2, bg 4)]

test_readEdgeListFile = testCase "Edge list file parsing #2" $ do
    el <- readEdgeListFile "test/data/sample-network.el" 
    el @?= v
    where
        v = V.fromList [ (bg 10, bg 11), (bg 10, bg 12), (bg 10, bg 13)
                       , (bg 11, bg 14), (bg 11, bg 15), (bg 11, bg 16)]

test_parseGenesetFile = testCase "Gene set file parsing #1" $
    parseGenesetFile "201\t0\t1|2|3" @?= v
    where
        v = V.fromList [(bgs 201 0, V.fromList [bg 1, bg 2, bg 3])]

test_readGenesetFile = testCase "Gene set file parsing #2" $ do
    gs <- readGenesetFile "test/data/sample-genesets.tsv"
    gs @?= v
    where
        v = V.fromList [ (bgs 246376 9606, V.fromList [bg 10, bg 11, bg 12])
                       , (bgs 75605 9606, V.fromList [bg 16, bg 15])
                       , (bgs 246626 9606, V.fromList [bg 13])
                       ]

test_parseAnnotationFile = testCase "Annotation file parsing #1" $
    parseAnnotationFile "ON:000\t1\tTAS\t0\nON:001\t2\tTAS\t0" @?= v
    where
        v = V.fromList [(bt "ON:000" "", bg 1), (bt "ON:001" "", bg 2)]

test_readAnnotationFile = testCase "Annotation file parsing #2" $ do
    as <- readAnnotationFile "test/data/sample-annotations.tsv"
    as @?= v
    where
        v = V.fromList [ (bt "GO:0051960" "", bg 11), (bt "GO:0051960" "", bg 16)
                       , (bt "GO:0007399" "", bg 11), (bt "GO:0007399" "", bg 16)
                       , (bt "GO:0070997" "", bg 12), (bt "GO:0070997" "", bg 11)
                       , (bt "GO:0070997" "", bg 16)
                       ]

test_parseTermFile = testCase "Term file parsing #1" $
    parseTermFile "ON:002\tON:001\nON:003\tON:001\nON:001\tON:000" @?= v
    where
        v = V.fromList [ (bt "ON:002" "", bt "ON:001" "")
                       , (bt "ON:003" "", bt "ON:001" "")
                       , (bt "ON:001" "", bt "ON:000" "")
                       ]

test_readTermFile = testCase "Term file parsing #2" $ do
    as <- readTermFile "test/data/sample-ontology.tsv"
    as @?= v
    where
        v = V.fromList [ (bt "GO:0051960" "", bt "GO:0007399" "")
                       , (bt "GO:0022008" "", bt "GO:0007399" "")
                       , (bt "GO:0021675" "", bt "GO:0007399" "")
                       , (bt "GO:0007399" "", bt "GO:0048731" "")
                       , (bt "GO:0031641" "", bt "GO:0051960" "")
                       ]

