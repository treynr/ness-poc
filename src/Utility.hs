
-- | file: Utility.hs
-- | desc: Miscellaneous utility functions.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}

module Utility where

import Data.Map     (Map)
import Data.Vector  (Vector)

import qualified Data.Map       as M
import qualified Data.Vector    as V

-- | Alias for vector concatenation.
--
(<+>) :: Vector a -> Vector a -> Vector a
--
(<+>) = (V.++)

-- | Don't try this at home kids, it's a bad partial function.
--
fromMaybe :: Maybe a -> a
--
fromMaybe (Just x) = x

-- | Don't do this either.
--
getIndex :: Ord k => k -> Map k a -> a
--
getIndex e = fromMaybe . M.lookup e


