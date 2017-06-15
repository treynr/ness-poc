
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

import Control.Applicative      ((<$>))
import Control.DeepSeq          (($!!), deepseq)
import Control.Monad            (when)
import Data.ByteString.Char8    (ByteString)
import Data.List                (intercalate, sortBy)
import Data.Map.Strict          (Map)
import Data.Matrix              (Matrix)
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
import qualified Data.Matrix        as MA
import qualified Data.Vector            as V
import qualified Data.Vector.Storable   as VS
--import qualified Numeric.LinearAlgebra.Data as LD

import Entity
import File
import Graph
--import Graph2 (updateAdjacencyList, updateAdjacencyList')
import Types
import Walk
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
optEdges = "Add the contents of the edge list file to the entity graph"
optGenesets = "Add the contents of the gene set file to the entity graph"
optAnnotations = "Add the contents of the annotation file to the entity graph"
optOntology = "Add ontology relationships to the entity graph"
optSimilarTo = "Calculate similarity for the given ontology term"
optTop = "Only include the top N most similar terms"
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
    , output = def &= argPos 0 &= typFile
}


---- Retrieves options and command line arguments specified by the user.
--
getOptions :: IO Options
getOptions = cmdArgs $ options
    -- &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= verbosity
    &= versionArg [explicit, C.name "version", summary _INFO]
    &= summary (_INFO)
    &= help _DESC
    &= helpArg [explicit, C.name "help", C.name "h"]
    &= program _NAME

main :: IO ()
main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) getOptions
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

    exec opts {
          edges = edges
        , genesets = genesets
        , annotations = annotations
        , ontology = ontology
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

{-
writeWalkedRelations :: FilePath -> Map Entity Int -> [Double] -> Entity -> [Entity] -> IO ()
--
writeWalkedRelations fp m ds e es = 
    B.appendFile fp (serializeWalkScores sims) >> B.appendFile fp "\n"
    where
        edexs = fmap (\e' -> (e', M.findWithDefault 0 e' m)) es
        sims = fmap (\(e', i) -> (e, e', ds !! i)) edexs

pairwiseWalk :: FilePath -> Matrix Double -> Map Entity Int -> [Entity] -> IO ()
--
pairwiseWalk _ _ _ [] = return ()
pairwiseWalk _ _ _ (e:[]) = return ()
pairwiseWalk fp ma m (e:es) 
    | eindex == 0 = pairwiseWalk fp ma m es
    | otherwise = writeWalkedRelations fp m walkSims e es >> pairwiseWalk fp ma m es
    where
        eindex = M.findWithDefault 0 e m
        walkSims = walk ma eindex
        -}

writeWalkedRelations :: FilePath -> Map Entity Int -> VS.Vector Double -> Entity -> IO ()
--
writeWalkedRelations fp m ds e = 
    B.appendFile fp (serializeWalkScores sims) >> B.appendFile fp "\n"
    where
        isTerm (ETerm _) = True
        isTerm _ = False
        -- Doing this every time is gonna be slow...
        termMap = M.filterWithKey (\k _ -> isTerm k) m
        entIndexes = M.foldlWithKey' (\ac e i -> (e, i) : ac) [] termMap
        sims' = fmap (\(e', i) -> (e, e', ds ! i)) entIndexes
        sims = filter (\(_, _, v) -> v > 0.0) sims'
        --edexs = V.map (\e' -> (e', M.findWithDefault 0 e' m)) es
        --sims = V.map (\(e', i) -> (e, e', ds ! i)) edexs

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
{-
--pairwiseWalk :: FilePath -> Matrix Double -> Map Entity Int -> Vector Entity -> IO ()
pairwiseWalk :: FilePath -> LD.Matrix Double -> Map Entity Int -> Vector Entity -> IO ()
--
--pairwiseWalk _ _ _ [] = return ()
--pairwiseWalk _ _ _ (e:[]) = return ()
pairwiseWalk fp ma m (uncons -> (vhead, vtail))
    | V.null vtail = return ()
    | eindex == 0 = pairwiseWalk fp ma m vtail
    | otherwise = writeWalkedRelations fp m walkSims vhead vtail >> pairwiseWalk fp ma m vtail
    -- | otherwise = do
    --     walk <- writeWalkedRelations fp m walkSims vhead vtail 
    --     walk `deepseq` pairwiseWalk fp ma m vtail
    where
        eindex = M.findWithDefault 0 vhead m
        walkSims = V.fromList $!! walk' ma eindex

pairwiseWalk2 :: FilePath -> LD.Matrix Double -> Map Entity Int -> Vector Entity -> IO ()
--
pairwiseWalk2 fp ma m (uncons -> (vhead, vtail))
    | V.null vtail = return ()
    | eindex == 0 = pairwiseWalk fp ma m vtail
    | otherwise = do
        putStrLn "doing a pairwise walk"
        appendFile "/projects/chesler-lab/walk-out.txt" "doing a pairwise walk\n"
        ws <- walkSims 
        writeWalkedRelations fp m ws vhead vtail
        pairwiseWalk fp ma m vtail
    where
        eindex = M.findWithDefault 0 vhead m
        ialkSims = V.fromList <$> walk'2 ma eindex
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

ofp = "/projects/chesler-lab/walk-out.txt"

---- Where all the execution magic happens. 
--
exec :: Options -> IO ()
exec opts@Options{..} = do 

    -- Verbosity argument
    verb <- isLoud

    B.writeFile ofp ""

    putStrLn "Reading files..."
    B.appendFile ofp "Reading files...\n"

    fEdges <- handleEdges edges
    fGenesets <- handleGenesets genesets
    fAnnotations <- handleAnnotations annotations
    fTerms <- handleOntology ontology

    putStrLn $ "Loaded " ++ (show $ V.length fEdges) ++ " network edges"
    putStrLn $ "Loaded " ++ (show $ V.length fGenesets) ++ " gene sets"
    putStrLn $ "Loaded " ++ (show $ V.length fAnnotations) ++ " ontology annotations"
    putStrLn $ "Loaded " ++ (show $ V.length fTerms) ++ " ontology relations"

    putStrLn "Manipulating stored entities..."

    let entities = flattenEntities fEdges fGenesets fAnnotations fTerms
    let graphSize = V.length entities

    --putStrLn $ show $ V.length entities
    --
    putStrLn $ (show $ V.length $ onlyTerms entities) ++ " unique terms"
    putStrLn $ (show $ V.length $ onlyGenes entities) ++ " unique genes"
    putStrLn $ (show $ V.length $ onlyGeneSets entities) ++ " unique gene sets"
    putStrLn $ (show $ graphSize) ++ " nodes"
    
    putStrLn "Tagging entities..."

    let entityIndex = tagEntities entities
    let indexEntity = M.foldlWithKey' (\ac k a -> M.insert a k ac) M.empty entityIndex

    -- Strictness is enforced in the rest of the code since we will eventually
    -- be using every single value (node) in the graph.
    entityIndex `deepseq` (B.appendFile ofp "Tagging entities...\n")
    --putStrLn $ show $ LD.size $ normalizeColumns' sampleGraph'

    -- let graphMatrix = updateAdjacencyMatrix False entityIndex fEdges $!!
    --let graphMatrix = convertMatrix $!! updateAdjacencyMatrix False entityIndex fEdges $!!
    --                  updateAdjacencyMatrix' True entityIndex fGenesets $!!
    --                  updateAdjacencyMatrix False entityIndex fAnnotations $!!
    --                  updateAdjacencyMatrix False entityIndex fTerms $!!
    --                  makeAdjacencyMatrix entities
    
    --let graphMatrix = LD.assoc ((V.length entities), (V.length entities)) 0.0 $!! 
    --                  updateAdjacencyList False entityIndex fEdges $!!
    --                  updateAdjacencyList' True entityIndex fGenesets $!!
    --                  updateAdjacencyList False entityIndex fAnnotations $!!
    --                  updateAdjacencyList False entityIndex fTerms $!!
    --                  []
    
    putStrLn "Building entity graph..."

    let graphMatrix = make1DMatrix (M.size entityIndex) $!! 
    --let graphMatrix = makeHPMatrix (M.size entityIndex) $!! 
    --let graphMatrix = makeMatrix (M.size entityIndex) $!! 
                      updateAdjacencyList False entityIndex fEdges $!! 
                      updateAdjacencyList' True entityIndex fGenesets $!!
                      updateAdjacencyList False entityIndex fAnnotations $!!
                      updateAdjacencyList False entityIndex fTerms []

    putStrLn "Column normalziing the graph matrix..."

    let graphMatrix' = normalize1DMatrix (M.size entityIndex) graphMatrix

    putStrLn "Forcing strictness..."

    graphMatrix' `deepseq` (B.appendFile ofp "Walking the graph...\n")


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

    if not $ null similarTo
    then do
        putStrLn $ "Finding terms similar to " ++ similarTo ++ "..."

        let termIndex = getIndex (termEntity (B.pack similarTo) "") entityIndex
        let result = randomWalk graphSize termIndex graphMatrix 0.15 (1.0 - 0.15)

        putStrLn "Transitioning to C code..."

        writeWalkedRelations output entityIndex result $ termEntity (B.pack similarTo) ""
        
    else do
        putStrLn "Transitioning to C code..."

        pairwiseWalk output entityIndex graphMatrix 0.15 $!! onlyTerms entities

    --pairwiseWalk2 output graphMatrix entityIndex $!! onlyTerms entities
{-
-}

    return ()

