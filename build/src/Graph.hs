
-- | file: Graph.hs
-- | desc: Adjacency matrix based graph representation using Maps and Sets.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Graph where

import Data.Map.Strict       (Map)
import Data.Set              (Set)
import Data.Tuple            (swap)
import Data.Vector           (Vector)
import System.Random.Shuffle (shuffleM)

import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import qualified Data.Vector          as V

import Entity (Entity(..))
import Utility

-- | Updates the an adjacency list with using the given list of 1:1 Entity
-- | associations (typically these are network edges). During construction of
-- | the adjacency list, this function converts entities into their matrix
-- | coordinates and adds it to the adjacency list. If the given boolean value
-- | is true, then undirected edges are added to the list.
--
updateAdjacencyList :: Bool -> 
                       Map Entity Int ->
                       Vector (Entity, Entity) -> 
                       [((Int, Int), Double)] ->
                       [((Int, Int), Double)]
--
updateAdjacencyList u m es as
    | u = V.foldl' (\ac (e1, e2) -> assoc e1 e2 : assoc e2 e1 : ac) as es
    | otherwise = V.foldl' (\ac (e1, e2) -> assoc e1 e2 : ac) as es
    where
        assoc a b = ((getIndex a m, getIndex b m), 1.0)

-- | Updates the an adjacency list with using the given list of 1:many Entity
-- | associations (typically these are network edges). During construction of
-- | the adjacency list, this function converts entities into their matrix
-- | coordinates and adds it to the adjacency list. If the given boolean value
-- | is true, then undirected edges are added to the list.
--
updateAdjacencyList' :: Bool ->
                        Map Entity Int -> 
                        Vector (Entity, Vector Entity) -> 
                        [((Int, Int), Double)] -> 
                        [((Int, Int), Double)]
--
updateAdjacencyList' u m es as
    | u = V.foldl' (\ac (e1, es') -> updateList' e1 es' ac) as es 
    | otherwise = V.foldl' (\ac (e1, es') -> updateList e1 es' ac) as es 
    where
        assoc a b = ((getIndex a m, getIndex b m), 1.0)
        updateList a bs as' = V.foldl' (\ac b -> assoc a b : ac) as' bs
        updateList' a bs as' = V.foldl' (\ac b -> assoc b a : assoc a b : ac) as' bs

-- | Builds a directed graph from the given list of bio entity relationships by
-- | mapping entities to their graph indices, then inserting those indices into
-- | the adjacency list.
-- |
-- | arguments
-- |    em: entity -> graph index mapping
-- |    ve: a vector of entity-entity relationships
-- |    g:  a graph represented as an adjacency list
--
buildDirectedGraph :: Map Entity Int ->
                      Vector (Entity, Entity) ->
                      Map Int (Set Int) ->
                      Map Int (Set Int)
--
buildDirectedGraph em ve g = V.foldl' (\g' (e1, e2) -> updateGraph e1 e2 g') g ve
    where
        updateGraph e1 e2 = M.insertWith (\o n -> S.union o n) (dex e1) (set e2)
        dex x = M.findWithDefault (-1) x em
        set = S.singleton . dex

-- | Similar to the buildDirectedGraph function but uses 1:many entity
-- | relationships to build the directed graph.
-- |
-- | arguments
-- |    em: entity -> graph index mapping
-- |    ve: a vector of 1:many entity relationships
-- |    g:  a graph represented as an adjacency list
--
buildDirectedGraph' :: Map Entity Int ->
                       Vector (Entity, Vector Entity) ->
                       Map Int (Set Int) -> 
                       Map Int (Set Int)
--
buildDirectedGraph' em ve g = V.foldl' (\g' (e, es) -> updateGraph e es g') g ve
    where
        updateGraph e es = M.insertWith (\o n -> S.union o n) (dex e) (set es)
        dex x = M.findWithDefault (-1) x em
        set = S.fromList . fmap dex . V.toList

-- | Builds an undirected entity graph from the given list of bio entity relationships by
-- | mapping entities to their graph indices, then inserting those indices into
-- | the adjacency list.
-- |
-- | arguments
-- |    em: entity -> graph index mapping
-- |    ve: a vector of entity-entity relationships
-- |    g:  a graph represented as an adjacency list
--
buildUndirectedGraph :: Map Entity Int -> 
                        Vector (Entity, Entity) ->
                        Map Int (Set Int) -> 
                        Map Int (Set Int)
--
buildUndirectedGraph em ve g = buildDirectedGraph em (fmap swap ve) $ 
                               buildDirectedGraph em ve g

-- | Like the buildUndirectedGraph function but uses 1:many entity relationships.
-- |
-- | arguments
-- |    em: entity -> graph index mapping
-- |    ve: a vector of entity-entity relationships
-- |    g:  a graph represented as an adjacency list
--
buildUndirectedGraph' :: Map Entity Int -> 
                         Vector (Entity, Vector Entity) ->
                         Map Int (Set Int) -> 
                         Map Int (Set Int)
--
buildUndirectedGraph' em ve g = let ug = buildDirectedGraph' em ve g
                                in  V.foldl' (\g' (e, es) -> updateGraph e es g') ug ve
    where
        updateGraph e es g' = 
            V.foldl' (\g'' e' -> M.insertWith S.union (dex e') (set e) g'') g' es
        dex x = M.findWithDefault (-1) x em
        set = S.singleton . dex 

-- | Uses the entity index map to ensure all entities are represented in the
-- | graph. If they are not, they are added although they will not have any
-- | outgoing edges (outdegree == 0). 
--
ensureGraphCompleteness :: Map Entity Int -> Map Int (Set Int) -> Map Int (Set Int)
-- 
ensureGraphCompleteness em g = M.foldl' (\g' x -> exists x g') g em
    where
        exists k g' = if M.member k g' then g' else M.insert k S.empty g'

-- | Updates dangling nodes, i.e., nodes that have an out-degree of zero. These
-- | nodes are updated to point to the special sink node.
--
updateDanglingNodes :: Int -> Map Int (Set Int) -> Map Int (Set Int)
--
updateDanglingNodes si g = M.foldlWithKey' (\g' k s -> ifEmpty k s g') g g
    where
        ifEmpty k s g' = if S.null s then M.insert k (S.singleton si) g' else g'

-- | Permute graph labels for permutation testing.
--
permuteGraphLabels :: Map Entity Int -> IO (Map Entity Int)
--
permuteGraphLabels me = do
    keys <- shuffleM $ M.keys me
    vals <- shuffleM $ M.elems me

    return $ M.fromList $ zip keys vals

