
-- | file: File.hs
-- | desc: File reading, writing, and parsing functions.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module File where

import Control.Applicative      ((<$>))
import Control.DeepSeq          (($!!))
import Data.Maybe               (fromMaybe)
import Data.ByteString.Char8    (ByteString)
import Data.Vector              (Vector)

import qualified Data.ByteString.Char8  as B
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as VS

import Types

serializeEntity :: Entity -> ByteString
--
serializeEntity (EGene g) = B.pack $ ("GENE:" ++) $ show $ ode g
serializeEntity (EGeneSet g) = B.pack $ ("GS:" ++) $ show $ gsid g
serializeEntity (ETerm t) = uid t
serializeEntity (Invalid) = "Invalid Entity"

serializeWalkScore :: (Entity, Entity, Double) -> ByteString
--
serializeWalkScore (e, f, d) = 
    B.intercalate "\t" [serializeEntity e, serializeEntity f, B.pack $ show d]

--serializeWalkScores :: [(Entity, Entity, Double)] -> ByteString
----
--serializeWalkScores = B.intercalate "\n" . fmap serializeWalkScore

--serializeWalkScores :: V.Vector (Entity, Entity, Double) -> ByteString
serializeWalkScores :: [(Entity, Entity, Double)] -> ByteString
--
--serializeWalkScores = B.intercalate "\n" . V.toList . V.map serializeWalkScore
serializeWalkScores = B.intercalate "\n" . fmap serializeWalkScore

removeEmpties :: [ByteString] -> [ByteString]
--
removeEmpties = filter (not . B.null)

-- Removes comment strings from the list.
--
removeComments :: [ByteString] -> [ByteString]
--
removeComments = filter (\b -> B.index b 0 /= '#')

-- | Parses the contents of an edge list file into a list of gene-gene edges
-- | and converts the Genes into Entity types.
-- | The row format for edge list files is:
-- |    
-- |    (0)      (1)       (2)
-- |    TAXON_ID NODE_FROM NODE_TO
--
--parseEdgeListFile :: ByteString -> [(Int, Entity, Entity)]
parseEdgeListFile :: ByteString -> Vector (Entity, Entity)
--
parseEdgeListFile bs = V.fromList $!! map (tuplify . B.split '\t') 
                       (removeComments $! removeEmpties $! B.lines bs)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        tuplify (a:b:_) = (EGene $! Gene $! toInt a, EGene $! Gene $! toInt b)
        --tuplify (s:a:b:_) = (toInt s, EGene $ Gene $ toInt a, EGene $ Gene $ toInt b)

-- | Reads and parses the edge list file.
--
readEdgeListFile :: FilePath -> IO (Vector (Entity, Entity))
--readEdgeListFile :: FilePath -> IO [(Int, Entity, Entity)]
--
readEdgeListFile fp = parseEdgeListFile <$> B.readFile fp

-- | Converts the given ByteString into Entity GeneSet types.
-- | The row format for geneset files is:
-- |    
-- |    (0)   (1)      (2)
-- |    GS_ID TAXON_ID ODE_GENE_ID(S)
-- |
-- | So the input list of ByteStrings should be three elements long.
-- | ode_gene_ids are separated by '|'.
--
toGeneset :: [ByteString] -> (Entity, Vector Entity)
--
toGeneset [a, b, c] =  (EGeneSet $! GeneSet (toInt a) (toInt b), geneSplit)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        geneSplit = V.fromList $!! map (EGene . Gene . toInt) $! B.split '|' c
toGeneset _ = (Invalid, V.empty)

-- | Parses the contents of a gene set file into a list of sets and associated
-- | genes.
--
parseGenesetFile :: ByteString -> Vector (Entity, Vector Entity)
--
parseGenesetFile bs = V.fromList $!! map (toGeneset . B.split '\t') $! removeComments $!
                      removeEmpties $! B.lines bs

-- | Reads and parses the gene set file.
--
readGenesetFile :: FilePath -> IO (Vector (Entity, Vector Entity))
--
readGenesetFile fp = parseGenesetFile <$> B.readFile fp

-- |    (0)     (1)         (2)           (3)
-- |    TERM_ID ODE_GENE_ID EVIDENCE_CODE TAXON_ID
--
toAnnotation :: [ByteString] -> (Entity, Entity)
--
toAnnotation [t, g, _, _] =  (term, gene)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        term = ETerm $! Term t ""
        gene = EGene $! Gene $! toInt g
toAnnotation _ = (Invalid, Invalid)

-- | Parses the contents of an annotation file, converting terms and
-- | ode_gene_ids into their respective entities and storing the relationship 
-- | as a tuple.
--
parseAnnotationFile :: ByteString -> Vector (Entity, Entity)
--
parseAnnotationFile bs = V.fromList $!! noInvalid $! map (toAnnotation . B.split '\t') $!
                         removeComments $! removeEmpties $! B.lines bs
    where
        noInvalid = filter (\(a, b) -> a /= Invalid && b /= Invalid)

-- | Reads and parses the annotation file.
--
readAnnotationFile :: FilePath -> IO (Vector (Entity, Entity))
--
readAnnotationFile fp = parseAnnotationFile <$> B.readFile fp

-- | Parses the contents of an edge list file into a list of gene-gene edges
-- | and converts the Genes into Entity types.
-- | The row format for edge list files is:
-- |    
-- |    (0)       (1)
-- |    NODE_FROM NODE_TO
--
parseTermFile :: ByteString -> Vector (Entity, Entity)
--
parseTermFile bs = V.fromList $!! map (tuplify . B.split '\t') 
                   (removeComments $! removeEmpties $! B.lines bs)
    where
        tuplify (a:b:_) = (ETerm $ Term a "", ETerm $ Term b "")

-- | Reads and parses the edge list file.
--
readTermFile :: FilePath -> IO (Vector (Entity, Entity))
--
readTermFile fp = parseTermFile <$> B.readFile fp

-- | (0)         (1)
-- | ENTITY_TYPE IDENTIFIER
--
toInput :: [ByteString] -> Entity
--
toInput bs
    | length bs /= 2 = Invalid
    | (bs !! 0) == "gene" = EGene $! Gene $! toInt $ bs !! 1
    | otherwise = ETerm $! (Term (B.intercalate ":" bs) "")
    -- | (bs !! 0) == "term" = ETerm $! (Term (bs !! 1) "")
    -- | otherwise = Invalid
    where
        toInt = fst . fromMaybe (0, "") . B.readInt

-- | Parses the contents of an input file, converting terms or ode_gene_ids
-- | into their respective entities and storing the relationship as a tuple.
--
parseInputFile :: ByteString -> [Entity]
--
parseInputFile bs = noInvalid $! map (toInput . B.split ':') $!
                    removeComments $! removeEmpties $! B.lines bs
    where
        noInvalid = filter (/= Invalid)

-- | Reads and parses a file containing a list of identifiers for use as input
-- | with the RWR.
--
readInputFile :: FilePath -> IO [Entity]
--
readInputFile fp = parseInputFile <$> B.readFile fp

-- | (0)         (1)
-- | ENTITY_TYPE IDENTIFIER
--
toOutputEntity :: [ByteString] -> Entity
--
toOutputEntity bs
    | length bs /= 2 = Invalid
    | (bs !! 0) == "gene" = EGene $! Gene $! toInt $ bs !! 1
    | otherwise = ETerm $! (Term (B.intercalate ":" bs) "")
    -- | (bs !! 0) == "term" = ETerm $! (Term (bs !! 1) "")
    -- | otherwise = Invalid
    where
        toInt = fst . fromMaybe (0, "") . B.readInt

-- | Parses the contents of an input file, converting terms or ode_gene_ids
-- | into their respective entities and storing the relationship as a tuple.
--
parseOutputFile :: ByteString -> [Entity]
--
parseOutputFile bs = noInvalid $! map (toOutputEntity . B.split ':'. head . B.split '\t') $!
                     removeComments $! removeEmpties $! B.lines bs
    where
        noInvalid = filter (/= Invalid)

-- | Reads and parses a file containing a list of identifiers for use as Output
-- | with the RWR.
--
readOutputFile :: FilePath -> IO [Entity]
--
readOutputFile fp = parseOutputFile <$> B.readFile fp
