
-- | file: Info.hs 
-- | desc: Program info and constants.
-- | vers: 0.1.0
-- | auth: TR
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Info (
   _DESC
 , _DTAG
 , _EXEC
 , _HASH
 , _INFO
 , _NAME
 , _VERS
) where

import Data.List          (intercalate)
import Data.Time          (getCurrentTime, toGregorian, utctDay)
import Development.GitRev (gitBranch, gitCommitCount, gitHash)
import System.Environment (getArgs)

_DESC :: String
_DESC = "Builds heterogeneous networks from functional genomics datasets"

_EXEC :: String
_EXEC = "nessb"

_HASH :: String
_HASH = $(gitBranch) ++ "@" ++ (take 8 $(gitHash))

_INFO :: String
_INFO = _EXEC ++ " v. " ++ _VERS ++ " (" ++ _HASH ++ ")"

_NAME :: String
_NAME = "NESS (Build)"

_VERS :: String
_VERS = "0.2." ++ $(gitCommitCount)

-- | Data export tag
-- | Attaches program version info and command line arguments for
-- | reproducibility, as well as the output file creation date.
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

