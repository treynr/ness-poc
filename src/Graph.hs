
-- | file: Graph.hs
-- | desc: Graph creation and node mapping functions.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}

module Graph where

import Data.List        (foldl')
import Data.Map.Strict  (Map)
import Data.Matrix      (Matrix, zero)

import qualified Data.Map.Strict as M
import qualified Data.Matrix as MA
import qualified Data.Set as S

import Types

-- | Removes duplicates from the list by inserting them into a Set and then
-- | converting back to a list.
--
removeDuplicates :: Ord a => [a] -> [a]
--
removeDuplicates = S.toList . S.fromList

-- | Given a list of entities, this function marks them with unique integer IDs
-- | that are 1-indexed for use in an adjacency matrix and returns the mapping
-- | of Entities -> IDs.
--
tagEntities :: [Entity] -> Map Entity Int
--
tagEntities es = M.fromList $! zip es [1 .. length es]

-- | Takes in genes, sets, and terms, removes duplicates from each list and
-- | converts them into Entity types.
--
convertEntities :: [Gene] -> [GeneSet] -> [Term] -> [Entity]
--
convertEntities gs gss ts = gs' ++ gss' ++ ts'
    where
        -- Maps each entity type to the actualy Entity type
        gs' = fmap EGene $! removeDuplicates gs
        gss' = fmap EGeneSet $! removeDuplicates gss
        ts' = fmap ETerm $! removeDuplicates ts

-- | Creates a zero filled, N x N adjacency matrix using the given list of
-- | entities.
--
makeAdjacencyMatrix :: [Entity] -> Matrix Double
--
makeAdjacencyMatrix es = zero les les
    where
        les = length es

-- | Updates the an adjacency matrix with edges derived from the given list of
-- | entity relationships. If the boolean u is True, the matrix is updated with
-- | undirected edges. 
--
updateAdjacencyMatrix :: Bool -> Map Entity Int -> [(Entity, Entity)] -> Matrix Double -> Matrix Double
--
updateAdjacencyMatrix _ _ [] m = m
updateAdjacencyMatrix u im ((e1, e2):es) m 
    | u = updateAdjacencyMatrix u im es $! setElement e2 e1 $! setElement e1 e2 m
    | otherwise = updateAdjacencyMatrix u im es $! setElement e1 e2 m
    where
        setElement a b = MA.setElem 1.0 (getIndex a im, getIndex b im)

-- | Updates the an adjacency matrix with edges derived from the given list of
-- | entity relationships. This function requires a 1:many mapping of entity 
-- | relationships. If the boolean u is True, the matrix is updated with
-- | undirected edges. 
--
updateAdjacencyMatrix' :: Bool -> Map Entity Int -> [(Entity, [Entity])] -> Matrix Double -> Matrix Double
--
updateAdjacencyMatrix' _ _ [] m = m
updateAdjacencyMatrix' u im ((e1, e2):es) m 
    | u = updateAdjacencyMatrix' u im es $! updateMatrix' e1 e2 $! updateMatrix e1 e2 m
    | otherwise = updateAdjacencyMatrix' u im es $! updateMatrix e1 e2 m
    where
        setElement a b = MA.setElem 1.0 (getIndex a im, getIndex b im)
        updateMatrix a bs m' = foldl' (flip (setElement a)) m' bs
        updateMatrix' a bs m' = foldl' (\ac b -> setElement b a ac) m' bs

--
-- Some helper functions for the updateAdjacencyMatrix functions.
--

-- | Don't try this at home kids, it's a bad partial function.
--
fromMaybe :: Maybe a -> a
--
fromMaybe (Just x) = x

-- | Don't do this either.
--
getIndex :: Entity -> Map Entity Int -> Int
--
getIndex e = fromMaybe . M.lookup e

