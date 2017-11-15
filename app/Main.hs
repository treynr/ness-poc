
-- | file:  Main.hs 
-- | desc:  Main stuffs: cmd line processing and program execution.
-- | vers:  0.1.0
-- | auth:  TR
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.DeepSeq          (($!!), deepseq)
import Control.Monad            (forM,forM_, join,when)
import Data.List                (intercalate, sortBy, sortOn)
import Data.List.Split          (splitOn)
import Data.Map.Strict          (Map)
import Data.Set                 (Set)
import Data.Time                (getCurrentTime, toGregorian, utctDay)
import Data.Vector              (Vector)
import Data.Vector.Storable     ((!))
import Development.GitRev       (gitBranch, gitCommitCount, gitHash)
import System.Console.CmdArgs
import System.Environment       (getArgs, withArgs)
import System.Exit              (ExitCode(..), exitWith)
import System.Random            (getStdRandom, randomR)
import System.Random.Shuffle    (shuffleM)

import qualified Data.ByteString.Char8  as B
-- Fucking name collision
import qualified System.Console.CmdArgs as C
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
--import qualified Data.Matrix        as MA
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as VS
--import qualified Numeric.LinearAlgebra.Data as LD

import Entity
import File
import Graph
import Types
import WalkFFI
import Utility

-- Cmd-line option shit
--
data Options = Options {

    -- Edge list file
    optEdges :: FilePath
    -- Gene set file
  , optGenesets :: FilePath
    -- Annotation file
  , optAnnotations :: [FilePath]
    -- File containing term-term relationships from an ontology
  , optOntology :: [FilePath]
    -- Calculate similarity for the given term
  , optSimilarTo :: String
    -- Calculate similarity for the given group of entities
  , optSimilarGroup :: String
    -- Top N most similar terms
  , optTop :: Int
    -- Restart probability
  , optRestart :: Double
    -- File w/ a list of identifiers to determine similarity 
  , optInputFile :: FilePath
    -- Exclude gene entities when saving output
  , optExcludeGenes :: Bool
    -- Exclude gene set entities when saving output
  , optExcludeSets :: Bool
    -- Exclude ontology term entities when saving output
  , optExcludeTerms :: Bool
    -- Generates graph permutations up to N for permutation testing
  , optPermute :: Int
    -- Adds X% of noise (false associations/edges) to the graph
  , optNoise :: Int
    -- Removes X% of associations/edges from the graph
  , optMissing :: Int
    -- Required argument: the output file data is saved to
  , argOutput :: FilePath

} deriving (Data, Eq, Show, Typeable)

_NAME :: String
_NAME = "Walker"

_EXEC :: String
_EXEC = "walker"

_VERS :: String
_VERS = "0.1." ++ $(gitCommitCount)

_HASH :: String
_HASH = $(gitBranch) ++ "@" ++ $(gitHash)

_INFO :: String
_INFO = _EXEC ++ " v. " ++ _VERS ++ " (" ++ _HASH ++ ")"

_DESC :: String
_DESC = "Ontology concept similarity using entity graphs and random walks with restart"

-- Data export tag
-- Attaches program version info and command line arguments for
-- reproducibility, as well as the output file creation date.
--
_DTAG :: String -> IO String
_DTAG cols = do
    sargs <- theArgs
    syear <- theYear
    smonth <- theMonth
    sday <- theDay

    return $ intercalate "\n"
        [ "## " ++ _INFO
        , "## last updated " ++ syear ++ ('.' : smonth) ++ ('.' : sday)
        , "## " ++ _EXEC ++ (' ' : sargs)
        , "## " ++ cols
        , "#"
        , "" ]
    where
        theDate' = (toGregorian . utctDay) <$> getCurrentTime
        theYear = (\(y, _, _) -> show y) <$> theDate'
        theMonth = (\(_, m, _) -> show m) <$> theDate'
        theDay = (\(_, _, d) -> show d) <$> theDate'
        theArgs = foldr (\acc xs -> acc ++ " " ++ xs) "" <$> getArgs

-- Text to display when viewing program options
--
txtEdges :: String
txtEdges = "Add the contents of the edge list file to the entity graph"

txtGenesets :: String
txtGenesets = "Add the contents of the gene set file to the entity graph"

txtAnnotations :: String
txtAnnotations = "Add the contents of the annotation file to the entity graph"

txtOntology :: String
txtOntology = "Add ontology relationships to the entity graph"

txtSimilarTo :: String
txtSimilarTo = "Calculate similarity for the given ontology term"

txtSimilarGroup :: String
txtSimilarGroup = "Calculate similarity for the given group of entities"

txtTop :: String
txtTop = "Only include the top N most similar terms"

txtRestart :: String
txtRestart = "Random walk restart probability (default a = 0.15)"

txtExcludeGenes :: String
txtExcludeGenes = "Exclude genes when saving output"

txtExcludeSets :: String
txtExcludeSets = "Exclude gene sets when saving output"

txtExcludeTerms :: String
txtExcludeTerms = "Exclude ontology terms when saving output"

txtInputFile :: String
txtInputFile = "File with a list of identifiers to determine similarity"

txtPermute :: String
txtPermute = "Generates graph permutations up to N for permutation testing"

txtNoise :: String
txtNoise = "Randomly add X% of false edges to the graph to simulate noise"

txtMissing :: String
txtMissing = "Randomly remove X% of edges from the graph to simulate missing information"

txtOutput :: String
txtOutput = "File to save data to"

---- Fills in info about the program's options.
--
options :: Options
options = Options {
      optEdges = def &= explicit &= C.name "e" &= C.name "edges" &= typFile &=
                 help txtEdges
    , optGenesets = def &= explicit &= C.name "g" &= C.name "genesets" &= 
                    typFile &= help txtGenesets
    , optAnnotations = def &= explicit &= C.name "a" &= C.name "annotations" &=
                       typFile &= help txtAnnotations
    , optOntology = def &= explicit &= C.name "o" &= C.name "ontology" &= 
                    typFile &= help txtOntology
    , optSimilarTo = def &= explicit &= C.name "similar-to" &= typ "STRING" &= 
                     help txtSimilarTo
    , optSimilarGroup = def &= explicit &= C.name "similar-group" &= 
                        typ "STRING" &= help txtSimilarGroup
    , optTop = def &= explicit &= C.name "top" &= typ "INT" &= help txtTop
    , optRestart = def &= explicit &= C.name "restart" &= typ "FLOAT" &= 
                   help txtRestart
    , optExcludeGenes = def &= explicit &= C.name "exclude-genes" &= 
                        typ "BOOL" &= help txtExcludeGenes
    , optExcludeSets = def &= explicit &= C.name "exclude-sets" &= 
                       typ "BOOL" &= help txtExcludeSets
    , optExcludeTerms = def &= explicit &= C.name "exclude-terms" &= 
                        typ "BOOL" &= help txtExcludeTerms
    , optInputFile = def &= explicit &= C.name "input-file" &= typFile &= 
                     help txtInputFile
    , optPermute = def &= explicit &= C.name "permute" &= typ "INT" &= 
                   help txtPermute
    , optNoise = def &= explicit &= C.name "noise" &= typ "INT" &= 
                   help txtNoise
    , optMissing = def &= explicit &= C.name "missing" &= typ "INT" &= 
                   help txtMissing
    , argOutput = def &= argPos 0 &= typFile
}


---- Retrieves options and command line arguments specified by the user.
--
getOptions :: IO Options
getOptions = cmdArgs $ options
    -- &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= verbosity
    &= versionArg [explicit, C.name "version", summary _INFO]
    &= summary _INFO
    &= help (_NAME ++ "\n" ++ _DESC)
    &= helpArg [explicit, C.name "help", C.name "h"]
    &= program _EXEC

main :: IO ()
main = do
    pargs <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null pargs then withArgs ["--help"] else id) getOptions
    optionHandler opts
 
---- Handles the user supplied options. Does some sanity checking and any 
---- malformed or missing options/arguments are dealt with.
--
optionHandler :: Options -> IO ()
optionHandler opts@Options{..}  = do

    when (null argOutput) $
        putStrLn "You must specify an output file" >> exitWith (ExitFailure 1)

    when (null optEdges && null optGenesets && null optAnnotations && null optOntology) $
        putStrLn "You must specify at least one of these options:" >>
        putStrLn "--edges" >> 
        putStrLn "--genesets" >>
        putStrLn "--annotations" >> 
        exitWith (ExitFailure 1)

    when (optRestart < 0.0 || optRestart >= 1.0) $
        putStrLn "The restart probability must be found in (0, 1)" >>
        exitWith (ExitFailure 1)

    exec opts {
        -- When no restart prob. is given we set the default to be 0.15
        optRestart = if optRestart <= 0.0 then 0.15 else optRestart
    }

--handleEdges :: FilePath -> IO [(Entity, Entity)]
handleEdges :: FilePath -> IO (Vector (Entity, Entity))
--
--handleEdges "" = return []
handleEdges "" = return V.empty
handleEdges fp = readEdgeListFile fp

--handleGenesets :: FilePath -> IO [(Entity, [Entity])]
handleGenesets :: FilePath -> IO (Vector (Entity, Vector Entity))
--
--handleGenesets "" = return []
handleGenesets "" = return V.empty
handleGenesets fp = readGenesetFile fp

--handleAnnotations :: FilePath -> IO [(Entity, Entity)]
handleAnnotations :: [FilePath] -> IO (Vector (Entity, Entity))
--
--handleAnnotations "" = return []
handleAnnotations [] = return V.empty
handleAnnotations fps = V.concat <$> (forM fps $ \f -> readAnnotationFile f)

--handleOntology :: FilePath -> IO [(Entity, Entity)]
--handleOntology :: FilePath -> IO (Vector (Entity, Entity))
handleOntology :: [FilePath] -> IO (Vector (Entity, Entity))
--
--handleOntology "" = return []
--handleOntology "" = return V.empty
--handleOntology fp = readTermFile fp
handleOntology [] = return V.empty
handleOntology fs = V.concat <$> (forM fs $ \f -> readTermFile f)

-- | Splits a comma delimited string into a list of strings.
--
convertCommaString :: String -> [String]
--
convertCommaString = fmap strip . splitOn ";"
    where
        strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

makeGOFilePath :: String -> FilePath
--
makeGOFilePath s
    | length split > 1 = (split !! 1) ++ ".tsv"
    | otherwise = s ++ ".tsv"
    where
        split = splitOn ":" s

removeGenesets :: Bool -> [(Entity, Double)] -> [(Entity, Double)]
--
removeGenesets False = id
removeGenesets _ = filter (not . isEGeneSet . fst)

removeTerms :: Bool -> [(Entity, Double)] -> [(Entity, Double)]
--
removeTerms False = id
removeTerms _ = filter (not . isETerm . fst)

removeGenes :: Bool -> [(Entity, Double)] -> [(Entity, Double)]
--
removeGenes False = id
removeGenes _ = filter (not . isEGene . fst)

removeSink :: [(Entity, Double)] -> [(Entity, Double)]
--
removeSink = filter (not . isSink . fst)

proxToEnts :: Map Entity Int -> VS.Vector Double -> [(Entity, Double)]
--
proxToEnts me ds = entList
    where
        entIndexes = M.foldlWithKey' (\ac e' i -> (e', i) : ac) [] me
        entList = fmap (\(e', i) -> (e', ds ! i)) entIndexes

writeWalkedRelations :: FilePath -> Map Entity Int -> VS.Vector Double -> Entity -> IO ()
--
writeWalkedRelations fp m ds e = 
    B.appendFile fp (serializeWalkScores sims) >> B.appendFile fp "\n"
    where
        isTerm (ETerm _) = True
        isTerm _ = False
        -- Doing this every time is gonna be slow...
        termMap = M.filterWithKey (\k _ -> isTerm k) m
        entIndexes = M.foldlWithKey' (\ac e' i -> (e', i) : ac) [] termMap
        sims' = fmap (\(e', i) -> (e, e', ds ! i)) entIndexes
        sims = sortBy (\(_, _, a) (_, _, b) -> compare b a ) $ filter (\(_, _, v) -> v > 0.0) sims'
        --edexs = V.map (\e' -> (e', M.findWithDefault 0 e' m)) es
        --sims = V.map (\(e', i) -> (e, e', ds ! i)) edexs
        --
writeWalkedRelations' :: FilePath -> Entity -> [(Entity, Double)] -> IO ()
--
writeWalkedRelations' fp e es = 
    B.appendFile fp (serializeWalkScores sims) >> B.appendFile fp "\n"
    where
        sims = fmap (\(e', d) -> (e, e', d)) $ filter (\t -> snd t > 0.0) es

-- | don't do this at home kids
--
uncons :: Vector a -> (a, Vector a)
--
uncons v = (V.unsafeHead v, V.unsafeTail v)

{-
pairwiseWalk :: FilePath -> Map Entity Int -> VS.Vector Double -> Double ->
                Vector Entity -> IO ()
--
pairwiseWalk fp me vs a (uncons -> (vh, vt))
    -- | V.null vh = return ()
    | V.null vt = writeWalkedRelations fp me walk vh
    | entIndex == -1 = pairwiseWalk fp me vs a vt
    | otherwise = writeWalkedRelations fp me walk vh >> pairwiseWalk fp me vs a vt
    where
        graphSize = M.size me
        entIndex = M.findWithDefault (-1) vh me
        walk = randomWalk graphSize entIndex vs a (1.0 - a)
-}

onlyTerms :: Vector Entity -> Vector Entity
--
onlyTerms = V.filter isTerm
    where
        isTerm (ETerm _) = True
        isTerm _ = False

onlyGeneSets :: Vector Entity -> Vector Entity
--
onlyGeneSets = V.filter isGS
    where
        isGS (EGeneSet _) = True
        isGS _ = False

onlyGenes :: Vector Entity -> Vector Entity
--
onlyGenes = V.filter isGene
    where
        isGene (EGene _) = True
        isGene _ = False

writeOutputHeader :: FilePath -> IO ()
--
writeOutputHeader fp = do 

    tag <- B.pack <$> _DTAG "NODE_FROM NODE_TO AFFINITY"

    B.writeFile fp tag
    B.appendFile fp "\n"

-- | Program output based on verbosity
--
scream :: Bool -> String -> IO ()
--
scream True s = putStrLn s
scream False _ = return ()

filterResults :: Options -> [(Entity, Double)] -> [(Entity, Double)]
--
filterResults Options{..} = removeSink .
                            removeGenes optExcludeGenes . 
                            removeGenesets optExcludeSets .
                            removeTerms optExcludeTerms

sortResults :: [(Entity, Double)] -> [(Entity, Double)]
--
sortResults = sortOn snd

permuteGraphLabels :: Map Entity Int -> IO (Map Entity Int)
--
permuteGraphLabels me = do
    keys <- shuffleM $ M.keys me
    vals <- shuffleM $ M.elems me

    return $ M.fromList $ zip keys vals

-- | Handles the --similar-to option which allows the user to specify one or
-- | more entities (usually ontology terms) and calculate similarity between
-- | these entities and all others
--
handleSimilarTo :: Options -> Map Entity Int -> VS.Vector Double -> IO ()
--
handleSimilarTo opts@Options{..} me graph
    | null optSimilarTo = return ()

    -- This will output RWR scores for permutations of the original graph
    --
    | optPermute > 1 = do

        writeOutputHeader argOutput

        forM_ (convertCommaString optSimilarTo) $ \term -> do

            forM_ [1 .. optPermute] $ \_ -> do
                me' <- permuteGraphLabels me

                let result = randomWalk msize 1 (vterm me' term) graph optRestart 
                let result' = sortResults $ filterResults opts $ proxToEnts me' result

                writeWalkedRelations' argOutput (termEntity (B.pack term) "") result'

    -- Normal walk, no permutation testing
    --
    | otherwise = do

        writeOutputHeader argOutput

        forM_ (convertCommaString optSimilarTo) $ \term -> do

            let result = randomWalk msize 1 (vterm me term) graph optRestart 
            let result' = sortResults $ filterResults opts $ proxToEnts me result

            writeWalkedRelations' argOutput (termEntity (B.pack term) "") result'
    where
        termIndex m t = getIndex (termEntity (B.pack t) "") m
        vterm m t = VS.singleton $ termIndex m t
        msize = M.size me

handleGroupPermutation :: Options -> Map Entity Int -> VS.Vector Double -> IO ()
--
handleGroupPermutation opts@Options{..} me graph
    | optPermute < 1 = return ()

    | not $ null optSimilarGroup = do

        writeOutputHeader argOutput

        -- For each permutation 1 -> N, we permute the graph labels, determine
        -- the new seed positions, then calculate the random walk.
        forM_ [1 .. optPermute] $ \_ -> do
            me' <- permuteGraphLabels me
            let group = fmap (\t -> getIndex (termEntity (B.pack t) "") me') $ convertCommaString optSimilarGroup
            let seeds = VS.fromList group

            let result = randomWalk (M.size me') (VS.length seeds) seeds graph optRestart 
            let result' = sortResults $ filterResults opts $ proxToEnts me' result

            writeWalkedRelations' argOutput (termEntity (B.pack optSimilarGroup) "") result'

    | otherwise = return ()

handleInputOptions :: Options -> Map Entity Int -> VS.Vector Double -> IO ()
--
handleInputOptions opts@Options{..} me graph
    | optPermute > 1 = return ()

    | not $ null optSimilarTo = do
        forM_ (convertCommaString optSimilarTo) $ \term -> do

            let termIndex = getIndex (termEntity (B.pack term) "") me
            let result = randomWalk (M.size me) 1 (VS.singleton termIndex) graph optRestart 
            let result' = sortResults $ filterResults opts $ proxToEnts me result

            writeOutputHeader (makeGOFilePath term)
            writeWalkedRelations' (makeGOFilePath term) (termEntity (B.pack term) "") result'

            --writeWalkedRelations (makeGOFilePath term) me result $ termEntity (B.pack term) ""

    | not $ null optSimilarGroup = do
        let group = fmap (\t -> getIndex (termEntity (B.pack t) "") me) $ convertCommaString optSimilarGroup
        let seeds = VS.fromList group

        let result = randomWalk (M.size me) (VS.length seeds) seeds graph optRestart 
        let result' = sortResults $ filterResults opts $ proxToEnts me result

        writeOutputHeader argOutput
        writeWalkedRelations' argOutput (termEntity (B.pack optSimilarGroup) "") result'

    | not $ null optInputFile = do

        inputs' <- separateMissingInputs me <$> readInputFile optInputFile

        if (length $ snd inputs') > 0 
        then do
            putStrLn "The following input entities are not present in the graph"
            putStrLn $ show $ B.intercalate ", " $ fmap entToBS $ snd inputs'
        else
            return ()

        writeOutputHeader argOutput

        forM_ (fst inputs') $ \ent -> do

            let termIndex = getIndex ent me
            let result = randomWalk (M.size me) 1 (VS.singleton termIndex) graph optRestart
            let result' = sortResults $ filterResults opts $ proxToEnts me result

            writeWalkedRelations' argOutput ent result'

    | otherwise = return ()
    where
        separateMissingInputs m ls = (onlyValidEntities m ls, noValidEntities m ls)
        onlyValidEntities m ls = filter (\k -> M.member k m) ls
        noValidEntities m ls = filter (\k -> not $ M.member k m) ls
        removeNonInputs s = filter (\(e, _) -> S.member e s) 
        entToBS (EGene g) = B.pack $ show $ ode g
        entToBS (EGeneSet g) = B.pack $ show $ gsid g
        entToBS (ETerm t) = uid t
        entToBS _ = "UNKNOWN"

handleNoise :: Options -> VS.Vector Double -> IO (VS.Vector Double)
--
handleNoise Options{..} vs
    | optNoise == 0 = return vs
    | otherwise = do
        updates <- addNoise -- numNoise vs
        --let updates = addNoise numNoise vs
        return $ VS.unsafeUpd vs updates
    where
        fi = fromIntegral
        vsl = VS.length vs
        rIndex = getStdRandom (randomR (0, vsl - 1))
        -- Number of edges that exist in the matrix
        numEdges = round $ VS.sum vs
        -- Amount of false edges to add based on % noise and existing edges
        numNoise = floor $ (fi numEdges) * ((fi optNoise) / 100.00)
        -- Vector of indices in the matrix that don't have an edge
        vNoEdges = VS.findIndices (== 0.0) vs
        --addNoise 0 vs' = return []
        --addNoise edgesLeft vs' = do
        addNoise = do
            edgesToUpdate <- shuffleM $ VS.toList vNoEdges

            return $ fmap (\i -> (i, 1.0)) $ take numNoise edgesToUpdate

            --return $ if (vs' ! idx) == 0.0
            ----then ((idx, 1.0) : addNoise (edgesLeft - 1) vs')
            --then (((idx, 1.0) :) <$> addNoise (edgesLeft - 1) vs')
            --else addNoise edgesLeft vs'

---- Where all the execution magic happens. 
--
exec :: Options -> IO ()
exec opts@Options{..} = do 

    -- Verbosity argument
    verb <- isLoud

    scream verb "Reading files..."

    fEdges <- handleEdges optEdges
    fGenesets <- handleGenesets optGenesets
    fAnnotations <- handleAnnotations optAnnotations
    fTerms <- handleOntology optOntology

    scream verb $ "Loaded " ++ show (V.length fEdges) ++ " network edges"
    scream verb $ "Loaded " ++ show (V.length fGenesets) ++ " gene sets"
    scream verb $ "Loaded " ++ show (V.length fAnnotations) ++ " ontology annotations"
    scream verb $ "Loaded " ++ show (V.length fTerms) ++ " ontology relations"

    scream verb "Manipulating stored entities..."

    --let entities = V.cons sinkEntity $!! flattenEntities fEdges fGenesets fAnnotations fTerms
    --let entities = flattenEntities fEdges fGenesets fAnnotations fTerms
    let entities = V.cons Sink $!! flattenEntities fEdges fGenesets fAnnotations fTerms
    let graphSize = V.length entities

    scream verb $ show (V.length $ onlyTerms entities) ++ " unique terms"
    scream verb $ show (V.length $ onlyGenes entities) ++ " unique genes"
    scream verb $ show (V.length $ onlyGeneSets entities) ++ " unique gene sets"
    scream verb $ show graphSize ++ " nodes"
    
    scream verb "Tagging entities..."

    let entityIndex = tagEntities entities
    let indexEntity = M.foldlWithKey' (\ac k a -> M.insert a k ac) M.empty entityIndex

    scream verb "Building entity graph..."

    --let graphMatrix = updateDanglingNodes (getIndex sinkEntity entityIndex) graphSize $!! 
    let graphMatrix = 
                      updateDanglingNodes (getIndex Sink entityIndex) graphSize $!!
                      make1DMatrix graphSize $!! 
                      updateAdjacencyList False entityIndex fEdges $!! 
                      updateAdjacencyList' True entityIndex fGenesets $!!
                      updateAdjacencyList True entityIndex fAnnotations $!!
                      updateAdjacencyList False entityIndex fTerms []

    scream verb "Column normalziing the graph matrix..."

    graphMatrix' <- handleNoise opts graphMatrix

    -- Strictness is enforced in the rest of the code since we will eventually
    -- be using every single value (node) in the graph.
    let graphMatrix'' = deepseq graphMatrix' $ normalize1DMatrix (M.size entityIndex) graphMatrix'

    if verb then putStr "Forcing strictness..." else return ()

    deepseq graphMatrix'' $ scream verb "done"

    handleInputOptions opts entityIndex graphMatrix''
    handleSimilarTo opts entityIndex graphMatrix''
    handleGroupPermutation opts entityIndex graphMatrix''

    scream verb "Done!"

    return ()

