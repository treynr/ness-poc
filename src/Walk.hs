
-- | file: Walk.hs
-- | desc: Random Walk with Restart (RWR) implementation.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}

module Walk where

import Control.Applicative  ((<$>))
import Data.Matrix          (Matrix, transpose)

import qualified Data.Matrix as MA
import qualified Data.Vector as V

alpha = 0.15
alpha' = 1.0 - alpha
threshold = 10e-12

sampleGraph = MA.fromLists [ [0.0, 1.0, 0.0, 1.0, 0.0, 1.0]
                           , [1.0, 0.0, 1.0, 1.0, 0.0, 0.0]
                           , [1.0, 1.0, 0.0, 0.0, 0.0, 0.0]
                           , [0.0, 1.0, 0.0, 0.0, 1.0, 0.0]
                           , [0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
                           , [0.0, 1.0, 0.0, 1.0, 0.0, 0.0] ]

-- | Calculates the L1-norm for the given vector.
--
l1Norm :: (Num a, Traversable t) => t a -> a
--
l1Norm = sum . fmap abs

-- | Performs L1 normalization on the given vector.
--
l1Normalization :: (Fractional a, Traversable t) => t a -> t a
--
l1Normalization vs = (/ norm) <$> vs
    where 
        norm = l1Norm vs

-- | Determine whether to keep the vector -> list conversion or replace
-- | everything with vector functions
-- | Performs column based L1 normalization on the entire matrix.
--
normalizeColumns :: Fractional a => Matrix a -> Matrix a
--
normalizeColumns m = transpose $ MA.fromLists $ normalize [1 .. MA.ncols m]
    where
        normalize = fmap (l1Normalization . (\i -> V.toList $ MA.getCol i m))

-- | Calculates the the initial proximity vector using a seed and matrix.
-- | Argument i is a seed node from which to start the search and the matrix is
-- | used to retrieve the number of nodes in the graph.
--
initialProxVector :: Fractional a => Int -> Matrix a -> [a]
--
initialProxVector i m
    | i < 1 || i > MA.nrows m =  1.0 : replicate (MA.nrows m - 1) 0.0
    | otherwise = replicate (i - 1) 0.0 ++ 1.0 : replicate (MA.nrows m - i) 0.0

-- | Might redo this to just multiply via map and vector indexing
-- | Multiply a matrix by a vector with the same number of rows
-- | e.g.
--      0 1 2
--      3 4 5 * <0 1 2 3 4 5 6 7 8>
--      6 7 8
--
matrixByVector m v = MA.toList $ transpose $ MA.multStd m $ 
                     MA.fromList (MA.nrows m) 1 v

-- | Generates a new proximity vector using the previous proximity vector,
-- | a restart vector, and matrix.
--
calculateProxVector pv rv m = zipWith (+) res eps
    where
        res = fmap (alpha *) rv
        eps = (alpha' *) <$> matrixByVector m pv

-- | Calculates the L1-norm difference between two vectors (previous and
-- | current) to determine whether the graph walk has converged.
--
calculateConvergence :: Num a => [a] -> [a] -> a
--
calculateConvergence pv pc = l1Norm $ zipWith (-) pv pc

-- | 1. Column normalize the matrix
-- | 2. Generate the initial proximity vector
-- | 3. Calculate the next proximity vector
-- | 4. Check for convergence
-- | 5. Rinse, repeat
--
-- | Performs a recursive graph walk on the graph m (adjacency matrix) 
-- | starting from a seed node s.
--
walk m s = walk' p0 $ calculateProxVector p0 p0 normMatrix
    where
        normMatrix = normalizeColumns m
        p0 = initialProxVector s normMatrix
        hasConverged p c = calculateConvergence p c < threshold
        walk' prev cur
            | hasConverged prev cur = walk' cur $ calculateProxVector cur p0 normMatrix
            | otherwise = cur

{-
l2 vs = fmap (/ norm) vs
    where 
        norm = sqrt $ sum $ fmap (\x -> x*x) vs
-}

