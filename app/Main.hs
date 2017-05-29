
-- | file:  Main.hs 
-- | desc:  Main stuffs: cmd line processing and program execution.
-- | vers:  0.1.0
-- | auth:  TR
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative      ((<$>))
import Control.Monad            (when)
import Data.List                (intercalate)
import Data.Time                (getCurrentTime, toGregorian, utctDay)
import System.Console.CmdArgs
import System.Environment       (getArgs, withArgs)
import System.Exit              (ExitCode(..), exitWith)

-- Cmd-line option shit
--
data Options = Options {

        -- Edge list file
        edges :: FilePath
        -- Gene set file
      , genesets :: FilePath
        -- Annotation file
      , annotations :: FilePath
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
_DESC = "Random walk with restart (RWR) among ontology concepts"

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
argOutput = "File to save data to"

---- Fills in info about the program's options.
--
options :: Options
options = Options {
      edges = def &= explicit &= name "edges" &= typFile &= help optEdges
    , genesets = def &= explicit &= name "genesets" &= typFile &= help optGenesets
    , annotations = def &= explicit &= name "annotations" &= typFile &= help optAnnotations
    , output = def &= argPos 0 &= typFile
}


---- Retrieves options and command line arguments specified by the user.
--
getOptions :: IO Options
getOptions = cmdArgs $ options
    -- &= verbosityArgs [explicit, name "verbose", name "v"] []
    &= verbosity
    &= versionArg [explicit, name "version", summary _INFO]
    &= summary (_INFO)
    &= help _DESC
    &= helpArg [explicit, name "help", name "h"]
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
        , output = output
    }

---- Where all the execution magic happens. 
--
exec :: Options -> IO ()
exec opts@Options{..} = do 

    -- Verbosity argument
    verb <- isLoud

    return ()

