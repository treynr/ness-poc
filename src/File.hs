
-- | file: File.hs
-- | desc: File reading, writing, and parsing functions.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module File where

import Control.Applicative      ((<$>))
import Data.Maybe               (fromMaybe)
import Data.ByteString.Char8    (ByteString)
import Data.Map.Strict          (Map)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M

import Types

-- | Parses the contents of an edge list file into a list of gene-gene edges.
--
parseEdgeListFile :: ByteString -> [(Gene, Gene)]
--
parseEdgeListFile bs = map (tuplify . B.split '\t') 
                       (removeComments $! removeEmpties $! B.lines bs)
    where
        removeEmpties = filter (not . B.null)
        removeComments = filter (\b -> B.index b 0 /= '#')
        toInt = fst . fromMaybe (0, "") . B.readInt
        tuplify ((!a):(!b):_) = (Gene $ toInt a, Gene $ toInt b)

-- | Reads and parses the edge list file.
--
readEdgeListFile :: FilePath -> IO [(Gene, Gene)]
--
readEdgeListFile fp = parseEdgeListFile <$> B.readFile fp

-- | The input list of ByteString should be three elements long: gs_id, sp_id,
-- | and ode_gene_ids which are separated by '|'.
--
toGeneset :: [ByteString] -> (GeneSet, [Gene])
--
toGeneset [a, b, c] =  (GeneSet (toInt a) (toInt b), geneSplit)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        geneSplit = map (Gene . toInt) $ B.split '|' c
toGeneset _ = (GeneSet 0 0, [])

-- | Parses the contents of a gene set file into a list of sets and associated
-- | genes.
--
parseGenesetFile :: ByteString -> [(GeneSet, [Gene])]
--
parseGenesetFile bs = map (toGeneset . B.split '\t') 
                      (removeComments $ removeEmpties $ B.lines bs)
    where
        removeEmpties = filter (not . B.null)
        removeComments = filter (\b -> B.index b 0 /= '#')

-- | Reads and parses the gene set file.
--
readGenesetFile :: FilePath -> IO [(GeneSet, [Gene])]
--
readGenesetFile fp = parseGenesetFile <$> B.readFile fp

