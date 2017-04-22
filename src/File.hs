
-- | file: File.hs
-- | desc: File reading, writing, and parsing functions.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module File where

import Control.Applicative      ((<$>))
import Data.Maybe               (fromMaybe)
import Data.ByteString.Char8    (ByteString)

import qualified Data.ByteString.Char8 as B

import Types

removeEmpties :: [ByteString] -> [ByteString]
--
removeEmpties = filter (not . B.null)

removeComments :: [ByteString] -> [ByteString]
--
removeComments = filter (\b -> B.index b 0 /= '#')

-- | Parses the contents of an edge list file into a list of gene-gene edges
-- | and converts the Genes into Entity types.
--
parseEdgeListFile :: ByteString -> [(Entity, Entity)]
--
parseEdgeListFile bs = map (tuplify . B.split '\t') 
                       (removeComments $! removeEmpties $! B.lines bs)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        tuplify (a:b:_) = (EGene $ Gene $ toInt a, EGene $ Gene $ toInt b)

-- | Reads and parses the edge list file.
--
readEdgeListFile :: FilePath -> IO [(Entity, Entity)]
--
readEdgeListFile fp = parseEdgeListFile <$> B.readFile fp

-- | Converts the given ByteString into Entity GeneSet types. The input list 
-- | of ByteStrings should be three elements long: gs_id, sp_id, and 
-- | ode_gene_ids which are separated by '|'.
--
toGeneset :: [ByteString] -> (Entity, [Entity])
--
toGeneset [a, b, c] =  (EGeneSet $! GeneSet (toInt a) (toInt b), geneSplit)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        geneSplit = map (EGene . Gene . toInt) $ B.split '|' c
toGeneset _ = (Invalid, [])

-- | Parses the contents of a gene set file into a list of sets and associated
-- | genes.
--
parseGenesetFile :: ByteString -> [(Entity, [Entity])]
--
parseGenesetFile = map (toGeneset . B.split '\t') . removeComments . 
                   removeEmpties . B.lines

-- | Reads and parses the gene set file.
--
readGenesetFile :: FilePath -> IO [(Entity, [Entity])]
--
readGenesetFile fp = parseGenesetFile <$> B.readFile fp

-- | Converts the given ByteString list into Entity Term and Gene types. The
-- | input list of ByteStrings should be three elements long: term id, term
-- | name and ode_gene_ids which are separated by '|'.
--
--toAnnotation :: [ByteString] -> (ByteString, [Gene])
toAnnotation :: [ByteString] -> (Entity, [Entity])
--
toAnnotation [a, b, c] =  (ETerm $! Term a b, geneSplit)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        geneSplit = map (EGene . Gene . toInt) $! B.split '|' b
toAnnotation _ = (Invalid, [])

-- | Parses the contents of an annotation file.
--
--parseAnnotationFile :: ByteString -> [(ByteString, [Gene])]
parseAnnotationFile :: ByteString -> [(Entity, [Entity])]
--
parseAnnotationFile = map (toAnnotation . B.split '\t') . removeComments .
                      removeEmpties . B.lines

-- | Reads and parses the annotation file.
--
readAnnotationFile :: FilePath -> IO [(Entity, [Entity])]
--
readAnnotationFile fp = parseAnnotationFile <$> B.readFile fp

