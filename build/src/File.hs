
-- | file: File.hs
-- | desc: File reading, writing, and parsing functions.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module File where

import Control.Applicative      ((<$>))
import Control.DeepSeq          (($!!))
import Control.Monad            (forM_)
import Data.Maybe               (fromMaybe)
import Data.ByteString.Char8    (ByteString)
import Data.List                (sortOn)
import Data.Map.Strict          (Map)
import Data.Set                 (Set)
import Data.Vector              (Vector)
import System.FilePath          ((</>), takeDirectory, takeFileName)
import System.IO                (withFile, IOMode(WriteMode))

import qualified Data.ByteString.Char8  as B
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import qualified Data.Vector            as V

import Entity ( Entity(..), buildGeneEntity, buildGeneSetEntity
              , buildHomologEntity, buildTermEntity, serializeEntity
              )

createEntityMapPath :: FilePath -> FilePath
--
createEntityMapPath fp = dir </> "entity-map-" ++ name
    where
        name = takeFileName fp
        dir = takeDirectory fp

-- | Removes null bytestrings from the list of bytestrings.
--
removeEmpties :: [ByteString] -> [ByteString]
--
removeEmpties = filter (not . B.null)

-- Removes comment strings from the list of bytestrings.
--
removeComments :: [ByteString] -> [ByteString]
--
removeComments = filter (\b -> B.index b 0 /= '#')

-- | Performs three preprocessing steps common to all file parsing: 
-- | splits the file into lines, removes empty lines, and removes 
-- | any comments.
--
preprocessFile :: ByteString -> [ByteString]
--
preprocessFile = removeComments . removeEmpties . B.lines

-- | Parses the contents of an edge list file into a list of gene-gene edges
-- | and converts the Genes into Entity types.
-- | The row format for edge list files is:
-- |    
-- |    (0)      (1)       (2)
-- |    TAXON_ID NODE_FROM NODE_TO
--
parseEdgeListFile :: ByteString -> Vector (Entity, Entity)
--
parseEdgeListFile bs = V.fromList $!! map (tuplify . B.split '\t') $ 
                       preprocessFile bs
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        tuplify (a:b:_) = (buildGeneEntity $ toInt a, buildGeneEntity $ toInt b)

-- | Reads and parses the edge list file.
--
readEdgeListFile :: FilePath -> IO (Vector (Entity, Entity))
--
readEdgeListFile fp = parseEdgeListFile <$> B.readFile fp

-- | Parses the contents of homology mapping file into a list of gene-homolog
-- | edges and converts everything into Entity types.
-- | The row format for edge list files is:
-- |    
-- |    (0)  (1)
-- |    GENE HOMOLOG
--
parseHomologFile :: ByteString -> Vector (Entity, Entity)
--
parseHomologFile bs = V.fromList $!! map (tuplify . B.split '\t') $
                      preprocessFile bs
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        tuplify (a:b:_) = (buildGeneEntity $ toInt a, buildHomologEntity $ toInt b)

-- | Reads and parses the homology mapping file.
--
readHomologFile :: FilePath -> IO (Vector (Entity, Entity))
--
readHomologFile fp = parseHomologFile <$> B.readFile fp

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
toGeneset [a, b, c] =  (buildGeneSetEntity (toInt a) (toInt b), geneSplit)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
        geneSplit = V.fromList $!! map (buildGeneEntity . toInt) $! B.split '|' c
toGeneset _ = (Invalid, V.empty)

-- | Parses the contents of a gene set file into a list of sets and associated
-- | genes.
--
parseGenesetFile :: ByteString -> Vector (Entity, Vector Entity)
--
parseGenesetFile bs = V.fromList $!! map (toGeneset . B.split '\t') $
                      preprocessFile bs

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
toAnnotation [t, g, _, _] =  (buildTermEntity t "", buildGeneEntity $ toInt g)
    where
        toInt = fst . fromMaybe (0, "") . B.readInt
toAnnotation _ = (Invalid, Invalid)

-- | Parses the contents of an annotation file, converting terms and
-- | ode_gene_ids into their respective entities and storing the relationship 
-- | as a tuple.
--
parseAnnotationFile :: ByteString -> Vector (Entity, Entity)
--
parseAnnotationFile bs = V.fromList $!! noInvalid $! 
                         map (toAnnotation . B.split '\t') $!
                         preprocessFile bs
                         
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
parseTermFile bs = V.fromList $!! map (tuplify . B.split '\t') $
                   preprocessFile bs
    where
        tuplify (a:b:_) = (buildTermEntity a "", buildTermEntity b "")

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
    | (bs !! 0) == "gene" = buildGeneEntity $ toInt $ bs !! 1
    | otherwise = buildTermEntity (B.intercalate ":" bs) ""
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
    | (bs !! 0) == "gene" = buildGeneEntity $ toInt $ bs !! 1
    | otherwise = buildTermEntity (B.intercalate ":" bs) ""
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

writeGraph :: FilePath -> Map Int (Set Int) -> IO ()
--
writeGraph fp m = withFile fp WriteMode $ \handle -> do
    
    B.hPutStrLn handle $ B.pack $ show $ M.size m

    forM_ (M.toAscList m) $ \(k, s) -> do
        B.hPutStrLn handle $ B.pack $ buildRowString s

    where
        buildRowString s = foldr (\x ac -> (if isIn x s then '1' else '0') : ',' : ac) "" [0..(size - 1)]
        isIn x s = S.member x s
        size = M.size m

writeSparseGraph :: FilePath -> Map Int (Set Int) -> IO ()
--
writeSparseGraph fp m = withFile fp WriteMode $ \handle -> do
    
    B.hPutStrLn handle $ B.pack $ show $ M.size m

    forM_ (M.toAscList m) $ \(k, s) -> do
        B.hPutStrLn handle $ buildRowString s

    where
        buildRowString s = B.intercalate "," $ fmap (B.pack . show) $ S.toList s

writeSparseGraph' :: FilePath -> Map Entity Int -> Map Int (Set Int) -> IO ()
--
writeSparseGraph' fp em g = withFile fp WriteMode $ \handle -> do
    
    B.hPutStrLn handle $ B.pack $ show $ M.size em

    forM_ (sortOn snd $ M.toAscList em) $ \(_, i) -> do
        B.hPutStrLn handle $ buildRowString $ getEdges i

    where
        buildRowString s = B.intercalate "," $ fmap (B.pack . show) $ S.toList s
        getEdges i = M.findWithDefault S.empty i g

writeEntityMap :: FilePath -> Map Entity Int -> IO ()
--
writeEntityMap fp em = withFile fp WriteMode $ \handle -> do

    B.hPutStrLn handle $ B.pack $ show $ M.size em

    forM_ (M.toAscList em) $ \(e, i) -> do
        B.hPutStrLn handle $ buildString e i

    where
        buildString e i = B.intercalate "\t" [serializeEntity e, B.pack $ show i]
    
