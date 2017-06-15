
-- | file: Graph.hs
-- | desc: Adjacency matrix based graph representation. Includes functions for
-- |       creating 1) pure Haskell, 2) high performance LAPACK/BLAS, and 3) 
-- |       memory efficient 1D vector implementations.
-- |       Use (1) for small graphs, (2) for medium/large graphs, and (3) for
-- |       huge graphs where memory is concern.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}

module Graph where

import Data.List        (foldl')
import Data.Map.Strict  (Map)
import Data.Matrix      (Matrix, zero)
import Data.Vector      (Vector)

import qualified Data.Map.Strict            as M
import qualified Data.Matrix                as MA
import qualified Data.Set                   as S
import qualified Data.Vector                as V
import qualified Data.Vector.Storable       as VS
--import qualified Numeric.LinearAlgebra.Data as LD

import Types
import Utility

-- | Updates the an adjacency list with using the given list of 1:1 Entity
-- | associations (typically these are network edges). During construction of
-- | the adjacency list, this function converts entities into their matrix
-- | coordinates and adds it to the adjacency list. If the given boolean value
-- | is true, then undirected edges are added to the list.
--
updateAdjacencyList :: Bool -> Map Entity Int -> Vector (Entity, Entity) -> 
                       [((Int, Int), Double)] -> [((Int, Int), Double)]
--
updateAdjacencyList u m es as
    | u = V.foldl' (\ac (e1, e2) -> (assoc e1 e2) : (assoc e2 e1) : ac) as es
    | otherwise = V.foldl' (\ac (e1, e2) -> (assoc e1 e2) : ac) as es
    where
        --assoc a b = ((getIndex a m, getIndex b m), 1.0)
        assoc a b = ((getIndex b m, getIndex a m), 1.0)

-- | Updates the an adjacency list with using the given list of 1:many Entity
-- | associations (typically these are network edges). During construction of
-- | the adjacency list, this function converts entities into their matrix
-- | coordinates and adds it to the adjacency list. If the given boolean value
-- | is true, then undirected edges are added to the list.
--
updateAdjacencyList' :: Bool -> Map Entity Int -> 
                        Vector (Entity, Vector Entity) -> 
                        [((Int, Int), Double)] -> [((Int, Int), Double)]
--
updateAdjacencyList' u m es as
    | u = V.foldl' (\ac (e1, es') -> updateList' e1 es' ac) as es 
    | otherwise = V.foldl' (\ac (e1, es') -> updateList e1 es' ac) as es 
    where
        --assoc a b = ((getIndex a m, getIndex b m), 1.0)
        assoc a b = ((getIndex b m, getIndex a m), 1.0)
        updateList a bs as' = V.foldl' (\ac b -> assoc a b : ac) as' bs
        updateList' a bs as' = V.foldl' (\ac b -> assoc b a : assoc a b : ac) as' bs

-- | Transforms the adjacency list into a pure Haskell based matrix.
--
makeMatrix :: Int -> [((Int, Int), Double)] -> Matrix Double
--
makeMatrix s = foldl' (\m (c, v) -> MA.setElem v c m) (MA.zero s s)

-- | Transforms the adjacency list into a high performance hmatrix for use with
-- | the linear algebra package.
--
--makeHPMatrix :: Int -> [((Int, Int), Double)] -> LD.Matrix Double
----
--makeHPMatrix s = LD.assoc (s, s) 0.0

-- | Transforms the adjacency list into a 1D vector representation.
--
make1DMatrix :: Int -> [((Int, Int), Double)] -> VS.Vector Double
--
make1DMatrix s as = VS.accum (+) (VS.replicate (s * s) 0.0) oneDim
    where
        oneDim = fmap (\((r, c), d) -> (s * r + c, d)) as

get1DRow :: Int -> Int -> VS.Vector Double -> VS.Vector Double
--
get1DRow i s vs
    | i < 0 || i > VS.length vs = VS.empty
    | otherwise = VS.slice (s * i) s vs

