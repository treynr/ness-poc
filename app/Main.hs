
-- | file:  Main.hs 
-- | desc:  Main stuffs: cmd line processing and program execution.
-- | vers:  0.1.0
-- | auth:  TR
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
        -- Required argument: the output file data is saved to
      , output :: FilePath

} deriving (Data, Eq, Show, Typeable)

_NAME :: String
_NAME = "Walker"

_EXEC :: String
_EXEC = "walker"

_VERS :: String
_VERS = "0.1.0"

_INFO :: String
_INFO = _EXEC ++ " version " ++ _VERS

_DESC :: String
_DESC = "Ontology concept similarity using entity graphs and random walk with restart"

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
        [ "## " ++ _NAME ++ " v. " ++ _VERS
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
--
optEdges = "Add the contents of the edge list file to the entity graph"

optGenesets :: String
--
optGenesets = "Add the contents of the gene set file to the entity graph"

optAnnotations :: String
--
optAnnotations = "Add the contents of the annotation file to the entity graph"

optOntology :: String
--
optOntology = "Add ontology relationships to the entity graph"

optSimilarTo :: String
--
optSimilarTo = "Calculate similarity for the given ontology term"

optTop :: String
--
optTop = "Only include the top N most similar terms"

optRestart :: String
--
optRestart = "Random walk restart probability (default a = 0.15)"

optSaveGenes :: String
--
optSaveGenes = "Save genes when creating output"

optSaveTerms :: String
--
optSaveTerms = "Save genes when creating output"

optInputFile :: String
--
optInputFile = "File with a list of identifiers to determine similarity"

argOutput :: String
--
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
    , top = def &= explicit &= C.name "top" &= typ "INT" &= help optTop
    , restart = def &= explicit &= C.name "restart" &= typ "FLOAT" &= help optRestart
    , saveGenes = def &= explicit &= C.name "save-genes" &= typ "BOOL" &= help optSaveGenes
    , saveTerms = def &= explicit &= C.name "save-terms" &= typ "BOOL" &= help optSaveTerms
    , inputFile = def &= explicit &= C.name "input-file" &= typFile &= help optInputFile
    , output = def &= argPos 0 &= typFile
}


---- Retrieves options and command line arguments specified by the user.
--
getOptions :: IO Options
getOptions = cmdArgs $ options
    -- &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= verbosity
    &= versionArg [explicit, C.name "version", summary _INFO]
    &= summary _INFO
    &= help _DESC
    &= helpArg [explicit, C.name "help", C.name "h"]
    &= program _NAME

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

handleInputOptions :: Options -> Map Entity Int -> VS.Vector Double -> IO ()
--
handleInputOptions Options{..} me graph
    | not $ null similarTo = do
        forM_ (handleSimilarTo similarTo) $ \term -> do

            --scream verb $ "Finding terms similar to " ++ term ++ "..."

            let termIndex = getIndex (termEntity (B.pack term) "") me
            let result = randomWalk (M.size me) termIndex graph restart (1.0 - restart)

            --scream verb "Transitioning to C code..."

            writeOutputHeader (makeGOFilePath term)

            writeWalkedRelations (makeGOFilePath term) me result $ termEntity (B.pack term) ""

    | not $ null inputFile = do
        inputs <- readInputFile inputFile
        verb <- isLoud

        let sinputs = S.fromList inputs

        writeOutputHeader output

        forM_ (inputs) $ \ent -> do

            let termIndex = getIndex ent me
            let result = randomWalk (M.size me) termIndex graph restart (1.0 - restart)
            let result' = removeNonInputs sinputs $ removeGenes (not saveGenes) $ removeTerms (not saveTerms) $ 
                          removeGenesets True $ proxToEnts me result

            writeWalkedRelations' output ent result'

    | otherwise = return ()
    where
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

    --writeOutputHeader output 


    --let thewalk = randomWalk (M.size entityIndex) (getIndex (ETerm (Term "GO:0008150" "")) entityIndex) graphMatrix' 0.15 (1 - 0.15)

    --putStrLn $ show $ V.filter (\(x, _) -> x == ETerm (Term "GO:0008150" "")) fTerms
    
    --putStrLn $ show $ V.filter (\(_, k) -> k>0.0) $ V.imap (\i x -> (getIndex i indexEntity, x)) $ V.convert $ VS.slice ((M.size entityIndex) * (getIndex (ETerm (Term "GO:0008150" "")) entityIndex)) (M.size entityIndex) graphMatrix
    --putStrLn $ show $ map (\(i, k) -> (getIndex i indexEntity, k)) $ filter (\(_, k) -> k>10e-20) $ zip [0 .. M.size entityIndex] $ concat $ LD.toLists $ graphMatrix LD.? [getIndex (ETerm (Term "GO:0008150" "")) entityIndex]
    --putStrLn $ show $ V.filter (\(_, k) -> k>0.0) $ V.imap (\i x -> (getIndex i indexEntity, x)) $ MA.getRow  (getIndex (ETerm (Term "GO:0008150" "")) entityIndex) graphMatrix

    --putStrLn $ show $ getIndex (ETerm (Term "GO:0000003" "")) entityIndex
    --putStrLn $ show $ getIndex (ETerm (Term "GO:0019953" "")) entityIndex
    --putStrLn $ show $ V.filter (\(_, k) -> k>0.0) $ V.imap (\i x -> (getIndex i indexEntity, x)) $ V.convert $ get1DRow (getIndex (ETerm (Term "GO:0000003" "")) entityIndex) graphSize graphMatrix
    --putStrLn $ show $ V.filter (\(_, k) -> k>0.0) $ V.imap (\i x -> (getIndex i indexEntity, x)) $ V.convert $ get1DRow (getIndex (ETerm (Term "GO:0019953" "")) entityIndex) graphSize graphMatrix
    --putStrLn $ show $ map (\(i, k) -> (getIndex i indexEntity, k)) $ filter (\(_, k) -> k>10e-20) $ zip [0 .. M.size entityIndex] $ concat $ LD.toLists $ graphMatrix LD.? [(getIndex (ETerm (Term "GO:0000003" "")) entityIndex)]
    --putStrLn $ show $ V.filter (\(_, k) -> k>0.0) $ V.imap (\i x -> (getIndex i indexEntity, x)) $ MA.getRow  (getIndex (ETerm (Term "GO:0000003" "")) entityIndex) graphMatrix

    --putStrLn $ show $ V.filter (\(_, k) -> k>0.0) $ V.imap (\i x -> (getIndex i indexEntity, x)) $ V.convert $ thewalk
    ----putStrLn $ show $ take 15 $ sortBy (flip compare) $ VS.toList thewalk
    --B.appendFile ofp (B.intercalate "\t" $ fmap (B.pack . show) $ VS.toList thewalk)
    --B.appendFile ofp "\n"

{-
    putStrLn $ show $ V.filter (\(_, k) -> k>0.0) $ 
               V.imap (\i x -> (getIndex i indexEntity, x)) $ V.convert $ 
               get1DRow (getIndex (ETerm (Term "GO:0005657" "")) entityIndex) graphSize graphMatrix

    putStrLn $ ("dangling nodes: " ++) $ show $ V.length $ V.filter (\(_, k) -> k>0.0) $ 
               V.imap (\i x -> (getIndex i indexEntity, x)) $ V.convert $ 
               get1DRow (getIndex sinkEntity entityIndex) graphSize graphMatrix
               -}

{-
    putStrLn $ show $ randomWalk 6 0 (normalize1DMatrix 6 sample1d) 0.15 (1.0 - 0.15)
    putStrLn $ show $ randomWalk 6 1 (normalize1DMatrix 6 sample1d) 0.15 (1.0 - 0.15)
    putStrLn $ show $ randomWalk 6 2 (normalize1DMatrix 6 sample1d) 0.15 (1.0 - 0.15)
    putStrLn $ show $ randomWalk 6 3 (normalize1DMatrix 6 sample1d) 0.15 (1.0 - 0.15)
    putStrLn $ show $ randomWalk 6 4 (normalize1DMatrix 6 sample1d) 0.15 (1.0 - 0.15)
    putStrLn $ show $ randomWalk 6 5 (normalize1DMatrix 6 sample1d) 0.15 (1.0 - 0.15)
-}

{-
    if not $ null similarTo
    then do
        forM_ (handleSimilarTo similarTo) $ \term -> do

            scream verb $ "Finding terms similar to " ++ term ++ "..."
            --scream verb $ VS.foldl' (+) 0.0 $ colSlice c $ VS.map (\r -> VS.slice (s * r + c) 1 vs ! 0) graphMatrix
        

            --scream verb $ show $ getIndex (termEntity (B.pack "GO:0008150") "") entityIndex
            --scream verb $ show $ getIndex (termEntity (B.pack similarTo) "") entityIndex

            let termIndex = getIndex (termEntity (B.pack term) "") entityIndex
            let result = randomWalk graphSize termIndex graphMatrix' restart (1.0 - restart)

            scream verb "Transitioning to C code..."

            writeOutputHeader (makeGOFilePath term)

            writeWalkedRelations (makeGOFilePath term) entityIndex result $ termEntity (B.pack term) ""
        
    else do
        scream verb "Transitioning to C code..."

        pairwiseWalk output entityIndex graphMatrix' restart $!! onlyTerms entities

    --pairwiseWalk2 output graphMatrix entityIndex $!! onlyTerms entities
-}

    handleInputOptions opts entityIndex graphMatrix'

    scream verb "Done!"

    return ()

