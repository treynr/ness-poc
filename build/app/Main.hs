
-- | file: Main.hs 
-- | desc: Main stuffs: cmd line processing and program execution.
-- | vers: 0.1.0
-- | auth: TR
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.DeepSeq         (($!!))
import Control.Monad           (when)
import Data.Map.Strict         ((!))
import System.Console.CmdArgs  (isLoud)
import System.Environment      (getArgs, withArgs)
import System.Exit             (ExitCode(..), exitWith)

import qualified Data.ByteString.Char8  as B
import qualified Data.Map.Strict        as M
import qualified Data.Set               as S
import qualified Data.Vector            as V

import CmdOptions ( Options(..), getOptions, handleAnnotations, handleEdges
                  , handleGenesets, handleHomologs, handleOntology
                  )
import Entity
import File
import Graph
import Info        (_DTAG)

main :: IO ()
main = do
    pargs <- getArgs
    -- If the user did not specify any arguments, pretend "--help" was given
    opts <- (if null pargs then withArgs ["--help"] else id) getOptions
    checkOptions opts

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

-- | Checks to ensure certain user supplied options are set.
--
checkOptions :: Options -> IO ()
--
checkOptions opts@Options{..}  = do

    when (null argOutput) $
        putStrLn "You must specify an output file" >> exitWith (ExitFailure 1)

    when (null optEdges && null optGenesets && null optAnnotations &&
          null optOntology) $
            putStrLn "You must specify at least one of these options:" >>
            putStrLn "-e/--edges" >> 
            putStrLn "-g/--genesets" >>
            putStrLn "-a/--annotations" >> 
            putStrLn "-o/--ontology" >> 
            exitWith (ExitFailure 1)

    exec opts

-- | Where all the execution magic happens. 
--
exec :: Options -> IO ()
--
exec opts@Options{..} = do 

    -- Verbosity argument
    verb <- isLoud

    scream verb "Reading files..."

    -- Read in various input files
    fEdges <- handleEdges optEdges
    fHomologs <- handleHomologs optHomologs
    fGenesets <- handleGenesets optGenesets
    fAnnotations <- handleAnnotations optAnnotations
    fTerms <- handleOntology optOntology

    scream verb $ "Loaded " ++ show (V.length fEdges) ++ " network edges"
    scream verb $ "Loaded " ++ show (V.length fHomologs) ++ " homology mappings"
    scream verb $ "Loaded " ++ show (V.length fGenesets) ++ " gene sets"
    scream verb $ "Loaded " ++ show (V.length fAnnotations) ++ " ontology annotations"
    scream verb $ "Loaded " ++ show (V.length fTerms) ++ " ontology relations"

    scream verb "Manipulating stored entities..."

    -- Merge bio entities into a single list and add a sink node for dangling
    -- nodes
    let entities = V.cons Sink $!! 
                   flattenEntities fEdges $
                   flattenEntities fHomologs $
                   flattenEntities' fGenesets $
                   flattenEntities fAnnotations $
                   flattenEntities fTerms V.empty

    let graphSize = V.length entities

    scream verb $ show (V.length $ onlyTerms entities) ++ " unique terms"
    scream verb $ show (V.length $ onlyGenes entities) ++ " unique genes"
    scream verb $ show (V.length $ onlyGeneSets entities) ++ " unique gene sets"
    
    scream verb "Tagging entities..."

    -- Assign zero-indexed IDs (for the adjacency matrix) to each unique entity
    let entityIndex = tagEntities entities

    -- The (!) function throws a runtime error if the key isn't found in the 
    -- map. This shouldn't happen with our Sink entity but if it does, 
    -- something went horrendously wrong.
    let sinkIndex = entityIndex ! Sink

    scream verb "Building entity graph..."

    preGraph <- simulateSparsity' optMissing =<< simulateNoise optNoise (
                    ensureGraphCompleteness entityIndex $
                    buildUndirectedGraph' entityIndex fGenesets $
                    if optDirected
                    then 
                        buildDirectedGraph entityIndex fHomologs  $
                        buildDirectedGraph entityIndex fTerms $
                        buildDirectedGraph entityIndex fAnnotations $
                        buildDirectedGraph entityIndex fEdges M.empty
                    else
                        buildUndirectedGraph entityIndex fHomologs $
                        buildUndirectedGraph entityIndex fTerms $
                        buildUndirectedGraph entityIndex fAnnotations $
                        buildUndirectedGraph entityIndex fEdges M.empty
                        )

    
    {--
    let preGraph = ensureGraphCompleteness entityIndex $
                    buildUndirectedGraph' entityIndex fGenesets $
                    if optDirected
                    then 
                        buildDirectedGraph entityIndex fHomologs  $
                        buildDirectedGraph entityIndex fTerms $
                        buildDirectedGraph entityIndex fAnnotations $
                        buildDirectedGraph entityIndex fEdges M.empty
                    else
                        buildUndirectedGraph entityIndex fHomologs $
                        buildUndirectedGraph entityIndex fTerms $
                        buildUndirectedGraph entityIndex fAnnotations $
                        buildUndirectedGraph entityIndex fEdges M.empty

    --}
    let numEdges = M.foldl' (\ac s -> ac + S.size s) 0 preGraph
    let toRemove = (fromIntegral numEdges) * (optMissing / 100.0)

    scream verb $ "toRemove: " ++ show toRemove
    scream verb $ show (M.foldl' (\ac s -> (S.size s) + ac) 0 preGraph) ++ " edges"

    --graph <- simulateSparsity' toRemove =<< simulateNoise optNoise (updateDanglingNodes sinkIndex preGraph)
    -- Build the (un)directed graph depending on user input
    -- Gene sets however are always undirected, I don't think it makes sense to
    -- use directed edges for them.
    let graph = updateDanglingNodes sinkIndex preGraph
    {-
    --}

    scream verb $ show graphSize ++ " nodes"
    scream verb $ show (M.foldl' (\ac s -> (S.size s) + ac) 0 graph) ++ " edges"
    scream verb "Doing some graph post-processing..."

    -- If the user specified --noise or --sparsity options, we add or remove
    -- edges respectively.
    --graph' <- simulateSparsity optMissing =<< simulateNoise optNoise graph
    let graph' = graph

    scream verb "Writing entity mapping to a file..."

    writeEntityMap (createEntityMapPath argOutput) entityIndex

    scream verb "Writing graph to a file..."

    --writeSparseGraph' argOutput entityIndex graph'
    --
    writeSparseGraph argOutput graph'
    --writeSparseGraph argOutput graph

    return ()

