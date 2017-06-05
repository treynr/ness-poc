
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
import Data.List                (intercalate)
import Data.Map.Strict          (Map)
import Data.Matrix              (Matrix)
import Data.Time                (getCurrentTime, toGregorian, utctDay)
import Data.Vector              (Vector, (!))
import System.Console.CmdArgs
import System.Environment       (getArgs, withArgs)
import System.Exit              (ExitCode(..), exitWith)

import qualified Data.ByteString.Char8  as B
-- Fucking name collision
import qualified System.Console.CmdArgs as C
import qualified Data.Map.Strict        as M
import qualified Data.Vector            as V
import qualified Numeric.LinearAlgebra.Data as LD

import File
import Graph
import Types
import Walk
import Walk2

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
argOutput = "File to save data to"

---- Fills in info about the program's options.
--
options :: Options
options = Options {
      edges = def &= explicit &= C.name "edges" &= typFile &= help optEdges
    , genesets = def &= explicit &= C.name "genesets" &= typFile &= help optGenesets
    , annotations = def &= explicit &= C.name "annotations" &= typFile &= help optAnnotations
    , ontology = def &= explicit &= C.name "ontology" &= typFile &= help optOntology
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

    when (null edges && null genesets && null annotations) $
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

writeWalkedRelations :: FilePath -> Map Entity Int -> Vector Double -> Entity -> Vector Entity -> IO ()
--
writeWalkedRelations fp m ds e es = 
    B.appendFile fp (serializeWalkScores sims) >> B.appendFile fp "\n"
    where
        edexs = V.map (\e' -> (e', M.findWithDefault 0 e' m)) es
        sims = V.map (\(e', i) -> (e, e', ds ! i)) edexs

-- | don't do this at home kids
--
uncons :: Vector a -> (a, Vector a)
--
uncons v = (V.unsafeHead v, V.unsafeTail v)

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
        ws <- walkSims 
        writeWalkedRelations fp m ws vhead vtail
        pairwiseWalk fp ma m vtail
    where
        eindex = M.findWithDefault 0 vhead m
        walkSims = V.fromList <$> walk'2 ma eindex

--onlyTerms :: [Entity] -> [Entity]
onlyTerms :: Vector Entity -> Vector Entity
--
onlyTerms = V.filter isTerm
    where
        isTerm (ETerm _) = True
        isTerm _ = False

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

    putStrLn $ show $ V.length fEdges
    putStrLn $ show $ V.length fGenesets
    putStrLn $ show $ V.length fAnnotations
    putStrLn $ show $ V.length fTerms

    {-
    entities <- flattenEntities <$> (handleEdges edges)
                                    (handleGenesets genesets)
                                    (handleAnnotations annotations)
                                    (handleOntology ontology)
    -}

    putStrLn "Building entity graph..."
    B.appendFile ofp "Buliding entity graph...\n"

    let entities = flattenEntities fEdges fGenesets fAnnotations fTerms

    putStrLn $ show $ V.length entities
    
    let entityIndex = tagEntities entities
    -- let graphMatrix = updateAdjacencyMatrix False entityIndex fEdges $!!
    let graphMatrix = convertMatrix $!! updateAdjacencyMatrix False entityIndex fEdges $!!
                      updateAdjacencyMatrix' True entityIndex fGenesets $!!
                      updateAdjacencyMatrix False entityIndex fAnnotations $!!
                      updateAdjacencyMatrix False entityIndex fTerms $!!
                      makeAdjacencyMatrix entities
    putStrLn "Walking the graph..."
    B.appendFile ofp "Walking the graph...\n"

    writeOutputHeader output 

    pairwiseWalk2 output graphMatrix entityIndex $!! onlyTerms entities
{-
    -}

    return ()

