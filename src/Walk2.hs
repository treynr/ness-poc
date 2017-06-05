
-- | file: Walk.hs
-- | desc: Random Walk with Restart (RWR) implementation.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}

module Walk2 where

import Control.Applicative          ((<$>))
import Control.DeepSeq              (($!!), deepseq)
-- import Numeric.LinearAlgebra.Data   (Matrix, Vector)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data   

--import qualified Data.Vector                as V
import qualified Numeric.LinearAlgebra.Data as LD
import System.IO

alpha = 0.15
alpha' = 1.0 - alpha
threshold = 10e-12

sampleGraph' :: Matrix Double
sampleGraph' = LD.fromLists [ [0.0, 1.0, 0.0, 1.0, 0.0, 1.0]
                           , [1.0, 0.0, 1.0, 1.0, 0.0, 0.0]
                           , [1.0, 1.0, 0.0, 0.0, 0.0, 0.0]
                           , [0.0, 1.0, 0.0, 0.0, 1.0, 0.0]
                           , [0.0, 1.0, 0.0, 0.0, 0.0, 0.0]
                           , [0.0, 1.0, 0.0, 1.0, 0.0, 0.0] ]

sampleV' :: Vector Double
sampleV' = LD.fromList [1,2,3,4,5,6]
sampleV1' :: Vector Double
sampleV1' = vector [1,2,1,1,3,4]
sampleV2' :: Vector Double
sampleV2' = vector [2,2,1,1,1,2]

-- | Calculates the L1-norm for the given vector.
--
l1Norm' :: Vector Double -> Double
--
-- l1Norm' = sum . cmap abs
l1Norm' = sumElements . cmap abs

-- | Performs L1 normalization on the given vector.
--
l1Normalization' ::  Vector Double -> Vector Double
--
l1Normalization' vs = cmap (/ norm) vs
    where 
        norm = l1Norm' vs

-- | matrix by vector
-- | m #> v
-- | Determine whether to keep the vector -> list conversion or replace
-- | everything with vector functions
-- | Performs column based L1 normalization on the entire matrix.
--
normalizeColumns' :: Matrix Double -> Matrix Double
--
normalizeColumns' m = fromColumns $ normalize m
    where
        --normalize = fmap (l1Normalization . (\i -> V.toList $ MA.getCol i m))
        normalize = fmap l1Normalization' . toColumns

-- | Calculates the the initial proximity vector using a seed and matrix.
-- | Argument i is a seed node from which to start the search and the matrix is
-- | used to retrieve the number of nodes in the graph.
--
initialProxVector' :: Int -> Int -> Vector Double
--
initialProxVector' i len
    | i < 0 || i > (len - 1) =  assoc len 0.0 [(0, 1.0)]
    | otherwise = assoc len 0.0 [(i, 1.0)]

{-
-- | Might redo this to just multiply via map and vector indexing
-- | Multiply a matrix by a vector with the same number of rows
-- | e.g.
--      0 1 2
--      3 4 5 * <0 1 2 3 4 5 6 7 8>
--      6 7 8
--
matrixByVector m v = MA.toList $ transpose $ MA.multStd m $ 
                     MA.fromList (MA.nrows m) 1 v
-}
-- | Generates a new proximity vector using the previous proximity vector,
-- | a restart vector, and matrix.
--
calculateProxVector' :: Vector Double -> Vector Double -> Matrix Double -> Vector Double
calculateProxVector' pv rv m = res + eps
    where
        res = alpha * rv
        eps = alpha' * (m #> pv)

-- | Calculates the L1-norm difference between two vectors (previous and
-- | current) to determine whether the graph walk has converged.
--
calculateConvergence' :: Vector Double -> Vector Double -> Double
--
calculateConvergence' pv pc = l1Norm' $!! pv - pc

-- | 1. Column normalize the matrix
-- | 2. Generate the initial proximity vector
-- | 3. Calculate the next proximity vector
-- | 4. Check for convergence
-- | 5. Rinse, repeat
--
-- | Performs a recursive graph walk on the graph m (adjacency matrix) 
-- | starting from a seed node s.
walk' :: Matrix Double -> Int -> [Double]
--
walk' m s = walk'' p0 $!! calculateProxVector' p0 p0 normMatrix
    where
        normMatrix = normalizeColumns' m
        p0 = initialProxVector' s $!! rows normMatrix
        hasConverged p c = calculateConvergence' p c < threshold
        walk'' prev cur
            | not $ hasConverged prev cur = walk'' cur $!! calculateProxVector' cur p0 normMatrix
            | otherwise = toList cur

walk'2 :: Matrix Double -> Int -> IO [Double]
--
walk'2 m s = walk'' p0 $!! calculateProxVector' p0 p0 normMatrix
    where
        normMatrix = normalizeColumns' m
        p0 = initialProxVector' s $!! rows normMatrix
        hasConverged p c = calculateConvergence' p c < threshold
        ps d = "convergence: " ++ show d
        walk'' prev cur
            | not $ hasConverged prev cur = 
                -- putStrLn (ps $ calculateConvergence' prev cur) >> 
                appendFile "/projects/chesler-lab/walk-out.txt" (ps $ calculateConvergence' prev cur) >> 
                appendFile "/projects/chesler-lab/walk-out.txt" "\n" >>
                (walk'' cur $!! calculateProxVector' cur p0 normMatrix)
            | otherwise = return $!! toList cur
{-
-}
{-
l2 vs = fmap (/ norm) vs
    where 
        norm = sqrt $ sum $ fmap (\x -> x*x) vs
-}

