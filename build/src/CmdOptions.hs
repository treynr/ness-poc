
-- | file: CmdOptions.hs 
-- | desc: Command line processing and handling.
-- | vers: 0.1.0
-- | auth: TR
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}

module CmdOptions (
   Options(..)
 , checkOptions
 , getOptions
 , handleAnnotations
 , handleEdges
 , handleGenesets
 , handleHomologs
 , handleOntology
) where

import Control.Applicative    ((<$>))
import Control.Monad          (forM, when)
import Data.Vector            (Vector)
import System.Console.CmdArgs ( Data, Typeable, (&=), argPos, cmdArgs, def
                              , explicit, help, helpArg, name, program 
                              , summary, typ, typFile, verbosity, versionArg
                              )
import System.Exit            (ExitCode(ExitFailure), exitWith)


import qualified Data.Vector as V

import Entity (Entity(..))
import File   ( readAnnotationFile, readEdgeListFile, readGenesetFile
              , readHomologFile, readTermFile
              )
import Info   (_DESC, _EXEC, _INFO, _NAME)

-- | Cmd-line options.
--
data Options = Options {

    -- Edge list files
    optEdges :: [FilePath]
    -- Gene set files
  , optGenesets :: [FilePath]
    -- Annotation files
  , optAnnotations :: [FilePath]
    -- File containing term-term relationships from an ontology
  , optOntology :: [FilePath]
    -- File containing homology mappings
  , optHomologs :: [FilePath]
    -- Build a directed graph
  , optDirected :: Bool
    -- Generates graph permutations up to N for permutation testing
  , optPermute :: Int
    -- Required argument: the output file the serialized graph is saved to
  , argOutput :: FilePath

} deriving (Data, Eq, Show, Typeable)

--
---- Text to display when viewing program options
--

txtEdges :: String
txtEdges = "Add the contents of the edge list file to the entity graph"

txtGenesets :: String
txtGenesets = "Add the contents of the gene set file to the entity graph"

txtAnnotations :: String
txtAnnotations = "Add the contents of the annotation file to the entity graph"

txtOntology :: String
txtOntology = "Add ontology relationships to the entity graph"

txtHomologs :: String
txtHomologs = "Add homology relationships to the entity graph"

txtDirected :: String
txtDirected = "Build a directed graph"

txtPermute :: String
txtPermute = "Generates graph permutations up to N for permutation testing"

-- | Fills in info about the program's options.
--
options :: Options
--
options = Options {
    optEdges       = def &= explicit &= name "e" &= name "edges" &= 
                     typFile &= help txtEdges
  , optGenesets    = def &= explicit &= name "g" &= name "genesets" &= 
                     typFile &= help txtGenesets
  , optAnnotations = def &= explicit &= name "a" &= name "annotations" &=
                     typFile &= help txtAnnotations
  , optOntology    = def &= explicit &= name "o" &= name "ontology" &= 
                     typFile &= help txtOntology
  , optHomologs    = def &= explicit &= name "h" &= name "homologs" &= 
                     typFile &= help txtHomologs
  , optDirected    = def &= explicit &= name "d" &= name "directed" &= 
                     typ "BOOL" &= help txtDirected
  , optPermute     = def &= explicit &= name "permute" &= typ "INT" &= 
                     help txtPermute
  , argOutput      = def &= argPos 0 &= typFile
}

-- | Retrieves options and command line arguments specified by the user.
--
getOptions :: IO Options
--
getOptions = cmdArgs $ options
    -- &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= verbosity
    &= versionArg [explicit, name "version", summary _INFO]
    &= summary _INFO
    &= help (_NAME ++ "\n" ++ _DESC)
    &= helpArg [explicit, name "help"]
    &= program _EXEC

-- | Checks to ensure certain user supplied options are set. An output file
-- | must always be specified and at least one data option must be used.
--
checkOptions :: Options -> IO ()
--
checkOptions Options{..} = do

    when (null argOutput) $
        putStrLn "You must specify an output file" >> exitWith (ExitFailure 1)

    let dataOptions = [optEdges, optGenesets, optAnnotations, optOntology, optHomologs]

    when (and $ fmap null dataOptions) $
            putStrLn "You must specify at least one of these options:" >>
            putStrLn "    -a/--annotations" >> 
            putStrLn "    -e/--edges" >> 
            putStrLn "    -h/--homologs" >>
            putStrLn "    -g/--genesets" >>
            putStrLn "    -o/--ontology" >> 
            exitWith (ExitFailure 1)

--
---- | These functions check to see if any input options (annotations, edges
---- | gene sets, homologs, or ontologies) are specified, then parses and 
---- | loads the files.
--

handleEdges :: [FilePath] -> IO (Vector (Entity, Entity))
--
handleEdges [] = return V.empty
handleEdges fs = V.concat <$> (forM fs $ \f -> readEdgeListFile f)

handleGenesets :: [FilePath] -> IO (Vector (Entity, Vector Entity))
--
handleGenesets [] = return V.empty
handleGenesets fs = V.concat <$> (forM fs $ \f -> readGenesetFile f)

handleAnnotations :: [FilePath] -> IO (Vector (Entity, Entity))
--
handleAnnotations [] = return V.empty
handleAnnotations fs = V.concat <$> (forM fs $ \f -> readAnnotationFile f)

handleOntology :: [FilePath] -> IO (Vector (Entity, Entity))
--
handleOntology [] = return V.empty
handleOntology fs = V.concat <$> (forM fs $ \f -> readTermFile f)

handleHomologs :: [FilePath] -> IO (Vector (Entity, Entity))
--
handleHomologs [] = return V.empty
handleHomologs fs = V.concat <$> (forM fs $ \f -> readHomologFile f)

