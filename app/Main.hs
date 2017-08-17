
-- | file:  Main.hs 
-- | desc:  Main stuffs: cmd line processing and program execution.
-- | vers:  0.1.0
-- | auth:  TR
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.DeepSeq          (($!!), deepseq)
import Control.Monad            (forM_, when)
import Data.List                (intercalate, sortBy)
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
--import Graph2 (updateAdjacencyList, updateAdjacencyList')
import Types
--import Walk
--import Walk2
import WalkFFI
import Utility

-- Cmd-line option shit
--
data Options = Options {

        -- Edge list file
        edges :: FilePath
        -- Gene set file
      , genesets :: FilePath
        -- Annotation file
      , annotations :: FilePath
        -- File containing term-term relationships from an ontology
      , ontology :: FilePath
        -- Calculate similarity for the given term
      , similarTo :: String
        -- Calculate similarity for the given group of entities
      , similarGroup :: String
        -- Top N most similar terms
      , top :: Int
        -- Restart probability
      , restart :: Double
        -- File w/ a list of identifiers to determine similarity 
      , inputFile :: FilePath
        -- Save genes when creating output
      , saveGenes :: Bool
        -- Save terms when creating output
      , saveTerms :: Bool
        -- Exclude gene entities when saving output
      , optExcludeGenes :: Bool
        -- Exclude gene set entities when saving output
      , optExcludeSets :: Bool
        -- Exclude ontology term entities when saving output
      , optExcludeTerms :: Bool
        -- Required argument: the output file data is saved to
      , output :: FilePath

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
optEdges :: String
optEdges = "Add the contents of the edge list file to the entity graph"

optGenesets :: String
optGenesets = "Add the contents of the gene set file to the entity graph"

optAnnotations :: String
optAnnotations = "Add the contents of the annotation file to the entity graph"

optOntology :: String
optOntology = "Add ontology relationships to the entity graph"

optSimilarTo :: String
optSimilarTo = "Calculate similarity for the given ontology term"

optSimilarGroup :: String
optSimilarGroup = "Calculate similarity for the given group of entities"

optTop :: String
optTop = "Only include the top N most similar terms"

optRestart :: String
optRestart = "Random walk restart probability (default a = 0.15)"

optSaveGenes :: String
optSaveGenes = "Save genes when creating output"

optSaveTerms :: String
optSaveTerms = "Save genes when creating output"

txtExcludeGenes :: String
txtExcludeGenes = "Exclude genes when saving output"

txtExcludeSets :: String
txtExcludeSets = "Exclude gene sets when saving output"

txtExcludeTerms :: String
txtExcludeTerms = "Exclude ontology terms when saving output"

optInputFile :: String
optInputFile = "File with a list of identifiers to determine similarity"

argOutput :: String
argOutput = "File to save data to"

---- Fills in info about the program's options.
--
options :: Options
options = Options {
      edges = def &= explicit &= C.name "edges" &= typFile &= help optEdges
    , genesets = def &= explicit &= C.name "genesets" &= typFile &= help optGenesets
    , annotations = def &= explicit &= C.name "annotations" &= typFile &= help optAnnotations
    , ontology = def &= explicit &= C.name "ontology" &= typFile &= help optOntology
    , similarTo = def &= explicit &= C.name "similar-to" &= typ "STRING" &= help optSimilarTo
    , similarGroup = def &= explicit &= C.name "similar-group" &= typ "STRING" &= help optSimilarGroup
    , top = def &= explicit &= C.name "top" &= typ "INT" &= help optTop
    , restart = def &= explicit &= C.name "restart" &= typ "FLOAT" &= help optRestart
    , saveGenes = def &= explicit &= C.name "save-genes" &= typ "BOOL" &= help optSaveGenes
    , saveTerms = def &= explicit &= C.name "save-terms" &= typ "BOOL" &= help optSaveTerms
    , optExcludeGenes = def &= explicit &= C.name "exclude-genes" &= typ "BOOL" &=help txtExcludeGenes
    , optExcludeSets = def &= explicit &= C.name "exclude-sets" &= typ "BOOL" &=help txtExcludeSets
    , optExcludeTerms = def &= explicit &= C.name "exclude-terms" &= typ "BOOL" &=help txtExcludeTerms
    , inputFile = def &= explicit &= C.name "input-file" &= typFile &= help optInputFile
    , output = def &= argPos 0 &= typFile
} -- &= summary _INFO &= program _EXEC


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

    when (null output) $
        putStrLn "You must specify an output file" >> exitWith (ExitFailure 1)

    when (null edges && null genesets && null annotations && null ontology) $
        putStrLn "You must specify at least one of these options:" >>
        putStrLn "--edges" >> 
        putStrLn "--genesets" >>
        putStrLn "--annotations" >> 
        exitWith (ExitFailure 1)

    when (restart < 0.0 || restart >= 1.0) $
        putStrLn "The restart probability must be found in (0, 1)" >>
        exitWith (ExitFailure 1)

    exec opts {
          edges = edges
        , genesets = genesets
        , annotations = annotations
        , ontology = ontology
        , restart = if restart <= 0.0 then 0.15 else restart
        , output = output
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
handleAnnotations :: FilePath -> IO (Vector (Entity, Entity))
--
--handleAnnotations "" = return []
handleAnnotations "" = return V.empty
handleAnnotations fp = readAnnotationFile fp

--handleOntology :: FilePath -> IO [(Entity, Entity)]
handleOntology :: FilePath -> IO (Vector (Entity, Entity))
--
--handleOntology "" = return []
handleOntology "" = return V.empty
handleOntology fp = readTermFile fp

handleSimilarTo :: String -> [String]
--
handleSimilarTo = fmap strip . splitOn ","
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
removeGenesets _ = filter (not . isSet . fst)
    where
        isSet (EGeneSet _) = True
        isSet _ = False

removeTerms :: Bool -> [(Entity, Double)] -> [(Entity, Double)]
--
removeTerms False = id
removeTerms _ = filter (not . isTerm . fst)
    where
        isTerm (ETerm _) = True
        isTerm _ = False

removeGenes :: Bool -> [(Entity, Double)] -> [(Entity, Double)]
--
removeGenes False = id
removeGenes _ = filter (not . isGene . fst)
    where
        isGene (EGene _) = True
        isGene _ = False

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
filterResults Options{..} = removeGenes optExcludeGenes . 
                               removeGenesets optExcludeSets .
                               removeTerms optExcludeTerms

handleInputOptions :: Options -> Map Entity Int -> VS.Vector Double -> IO ()
--
handleInputOptions opts@Options{..} me graph
    | not $ null similarTo = do
        forM_ (handleSimilarTo similarTo) $ \term -> do

            let termIndex = getIndex (termEntity (B.pack term) "") me
            let result = randomWalk (M.size me) 1 (VS.singleton termIndex) graph restart 
            let result' = filterResults opts $ proxToEnts me result

            writeOutputHeader (makeGOFilePath term)
            writeWalkedRelations' (makeGOFilePath term) (termEntity (B.pack term) "") result'

            --writeWalkedRelations (makeGOFilePath term) me result $ termEntity (B.pack term) ""

    | not $ null similarGroup = do
        let group = fmap (\t -> getIndex (termEntity (B.pack t) "") me) $ handleSimilarTo similarGroup
        let seeds = VS.fromList group

        let result = randomWalk (M.size me) 1 seeds graph restart 

            --scream verb "Transitioning to C code..."

        writeOutputHeader output

        writeWalkedRelations output me result $ termEntity (B.pack similarGroup) ""

    | not $ null inputFile = do
        inputs' <- separateMissingInputs me <$> readInputFile inputFile
        --inputs <- readInputFile inputFile
        --let inputs' = separateMissingInputs me inputs
        verb <- isLoud

        if (length $ snd inputs') > 0 
        then do
            putStrLn "The following input entities are not present in the graph"
            putStrLn $ show $ snd inputs'
        else
            return ()

        writeOutputHeader output

        forM_ (fst inputs') $ \ent -> do

            let termIndex = getIndex ent me
            let result = randomWalk (M.size me) 1 (VS.singleton termIndex) graph restart
            let result' = removeGenes (not saveGenes) $ removeTerms (not saveTerms) $ 
                          removeGenesets True $ proxToEnts me result

            writeWalkedRelations' output ent result'

    | otherwise = return ()
    where
        separateMissingInputs m ls = (onlyValidEntities m ls, noValidEntities m ls)
        onlyValidEntities m ls = filter (\k -> M.member k m) ls
        noValidEntities m ls = filter (\k -> not $ M.member k m) ls
        removeNonInputs s = filter (\(e, _) -> S.member e s) 

---- Where all the execution magic happens. 
--
exec :: Options -> IO ()
exec opts@Options{..} = do 

    -- Verbosity argument
    verb <- isLoud

    scream verb "Reading files..."

    fEdges <- handleEdges edges
    fGenesets <- handleGenesets genesets
    fAnnotations <- handleAnnotations annotations
    fTerms <- handleOntology ontology

    scream verb $ "Loaded " ++ show (V.length fEdges) ++ " network edges"
    scream verb $ "Loaded " ++ show (V.length fGenesets) ++ " gene sets"
    scream verb $ "Loaded " ++ show (V.length fAnnotations) ++ " ontology annotations"
    scream verb $ "Loaded " ++ show (V.length fTerms) ++ " ontology relations"

    scream verb "Manipulating stored entities..."

    --let entities = V.cons sinkEntity $!! flattenEntities fEdges fGenesets fAnnotations fTerms
    let entities = flattenEntities fEdges fGenesets fAnnotations fTerms
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
                      make1DMatrix graphSize $!! 
                      updateAdjacencyList False entityIndex fEdges $!! 
                      updateAdjacencyList' True entityIndex fGenesets $!!
                      updateAdjacencyList True entityIndex fAnnotations $!!
                      updateAdjacencyList False entityIndex fTerms []

    scream verb "Column normalziing the graph matrix..."

    -- Strictness is enforced in the rest of the code since we will eventually
    -- be using every single value (node) in the graph.
    let graphMatrix' = deepseq graphMatrix $ normalize1DMatrix (M.size entityIndex) graphMatrix

    if verb then putStr "Forcing strictness..." else return ()

    deepseq graphMatrix' $ scream verb "done"

    handleInputOptions opts entityIndex graphMatrix'

    scream verb "Done!"

    return ()

