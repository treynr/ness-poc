
-- | file: Graph.hs
-- | desc: Graph creation and node mapping functions.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Graph2 where

import Control.DeepSeq          (($!!), deepseq)
import Data.List        (foldl')
import Data.Map.Strict  (Map)
import Data.Matrix      (Matrix, zero)
import Data.Vector      (Vector)

import qualified Data.Map.Strict    as M
import qualified Data.Matrix        as MA
import qualified Data.Set           as S
import qualified Data.Vector        as V
import qualified Numeric.LinearAlgebra.Data as LD

import Types
import Utility

convertMatrix :: Matrix Double -> LD.Matrix Double
--
convertMatrix m = LD.fromLists asList
    where
        asList = fmap ((\i -> V.toList $ MA.getRow i m)) [1 .. MA.nrows m]

-- | Creates a zero filled, N x N adjacency matrix using the given list of
-- | entities.
--
makeAdjacencyMatrix :: Vector Entity -> Matrix Double
--
makeAdjacencyMatrix es = zero les les
    where
        les = V.length es

-- | Updates the an adjacency matrix with edges derived from the given list of
-- | entity relationships. If the boolean u is True, the matrix is updated with
-- | undirected edges. 
--
updateAdjacencyMatrix :: Bool -> Map Entity Int -> Vector (Entity, Entity) -> 
                         Matrix Double -> Matrix Double
--
updateAdjacencyMatrix u m es ma
    | u = V.foldl (\ac (e1, e2) -> setElement e2 e1 $! setElement e1 e2 ac) ma es
    | otherwise = V.foldl (\ac (e1, e2) -> setElement e1 e2 ac) ma es
    where
        setElement a b = MA.setElem 1.0 (getIndex a m, getIndex b m)

-- | Updates the an adjacency matrix with edges derived from the given list of
-- | entity relationships. This function requires a 1:many mapping of entity 
-- | relationships. If the boolean u is True, the matrix is updated with
-- | undirected edges. 
--
updateAdjacencyMatrix' :: Bool -> Map Entity Int -> 
                          Vector (Entity, Vector Entity) -> Matrix Double -> 
                          Matrix Double
--
updateAdjacencyMatrix' u m es ma
    | u = V.foldl (\ac (e1, e2) -> updateMatrix' e1 e2 $! updateMatrix e1 e2 ac) ma es --updateAdjacencyMatrix' u im es $! updateMatrix' e1 e2 $! updateMatrix e1 e2 m
    | otherwise = V.foldl (\ac (e1, e2) -> updateMatrix e1 e2 ac) ma es --updateAdjacencyMatrix' u im es $! updateMatrix e1 e2 m
    where
        setElement a b = MA.setElem 1.0 (getIndex a m, getIndex b m)
        updateMatrix a bs ma' = V.foldl' (flip (setElement a)) ma' bs
        updateMatrix' a bs ma' = V.foldl' (\ac b -> setElement b a ac) ma' bs

updateAdjacencyList :: Bool -> Map Entity Int -> Vector (Entity, Entity) -> [((Int, Int), Double)] -> [((Int, Int), Double)]

updateAdjacencyList u m es as
    | u = V.foldl' (\ac (e1, e2) -> (makeAssociation e1 e2) : (makeAssociation e2 e1) : ac) as es
    | otherwise = V.foldl' (\ac (e1, e2) -> (makeAssociation e1 e2) : ac) as es
    where
        makeAssociation a b = ((getIndex a m, getIndex b m), 1.0)

updateAdjacencyList' :: Bool -> Map Entity Int -> 
                          Vector (Entity, Vector Entity) -> [((Int, Int), Double)] -> 
                          [((Int, Int), Double)]
--
updateAdjacencyList' u m es as
    | u = V.foldl' (\ac (e1, es') -> updateList' e1 es' ac) as es 
    | otherwise = V.foldl' (\ac (e1, es') -> updateList e1 es' ac) as es 
    where
        makeAssociation a b = ((getIndex a m, getIndex b m), 1.0)
        updateList a bs as' = V.foldl' (\ac b -> makeAssociation a b : ac) as' bs
        updateList' a bs as' = V.foldl' (\ac b -> makeAssociation b a : makeAssociation a b : ac) as' bs

