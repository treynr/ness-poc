
-- | file: Graph.hs
-- | desc: Adjacency matrix based graph representation. Includes functions for
-- |       creating pure Haskell and memory efficient 1D vector
-- |       implementations.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}

module Graph where

import Data.List        (foldl')
import Data.Map.Strict  (Map)
import Data.Vector      (Vector)

import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

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
    | u = V.foldl' (\ac (e1, e2) -> assoc e1 e2 : assoc e2 e1 : ac) as es
    | otherwise = V.foldl' (\ac (e1, e2) -> assoc e1 e2 : ac) as es
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

updateDanglingNodes :: Int -> Int -> VS.Vector Double -> VS.Vector Double
--
updateDanglingNodes si s vs = (VS.//) vs $ fmap (\i -> (s * si + i, 1.0)) $! 
                              foldl' (\ac c -> if colSum c > 0.0 then ac else c : ac) [] [0 .. (s - 1)]
    where
        velems = VS.fromList [0 .. (s - 1)]
        colSlice c = VS.map (\r -> VS.slice (s * r + c) 1 vs VS.! 0) velems
        colSum c = VS.foldl' (+) 0.0 $ colSlice c 

-- | Transforms the adjacency list into a 1D vector representation.
--
make1DMatrix :: Int -> [((Int, Int), Double)] -> VS.Vector Double
--
make1DMatrix s as = (VS.//) (VS.replicate (s * s) 0.0) oneDim
    where
        oneDim = fmap (\((r, c), d) -> (s * r + c, d)) as

get1DRow :: Int -> Int -> VS.Vector Double -> VS.Vector Double
--
get1DRow i s vs
    | i < 0 || i > VS.length vs = VS.empty
    | otherwise = VS.slice (s * i) s vs

