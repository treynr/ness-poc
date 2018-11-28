
-- | file: Graph.hs
-- | desc: Adjacency matrix based graph representation. Includes functions for
-- |       creating pure Haskell and memory efficient 1D vector
-- |       implementations.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Graph where

import Data.List             (foldl', sortBy)
import Data.Map.Strict       (Map)
import Data.Set              (Set)
import Data.Tuple            (swap)
import Data.Vector           (Vector)
import System.Random         (randomRIO)
import System.Random.Shuffle (shuffleM)

import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Entity
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

--updateDanglingNodes :: Int -> Int -> VS.Vector Double -> VS.Vector Double
----
--updateDanglingNodes si s vs = (VS.//) vs $ fmap (\i -> (s * si + i, 1.0)) $! 
--                              foldl' (\ac c -> if colSum c > 0.0 then ac else c : ac) [] [0 .. (s - 1)]
--    where
--        velems = VS.fromList [0 .. (s - 1)]
--        colSlice c = VS.map (\r -> VS.slice (s * r + c) 1 vs VS.! 0) velems
--        colSum c = VS.foldl' (+) 0.0 $ colSlice c 

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

-- | Builds the Entity graph in a format for serialization
--
--buildGraph :: Map Entity Int -> Vector (Entity, Entity) 
--           -> Vector (Entity, Vector Entity) -> Map Int (Set Int)
----
--buildGraph em edges genesets = M.unionWith (S.union) bothEdgeVectors bothGSVectors
--    where
--        bothEdgeVectors = M.unionWith S.union addEdgeVector addEdgeVector'
--        bothGSVectors = M.unionWith S.union addGSVector addGSVector'
--        addEdgeVector = V.foldl' (\am (e1, e2) -> updateMap e1 e2 am) M.empty edges
--        addEdgeVector' = V.foldl' (\am (e1, e2) -> updateMap e2 e1 am) M.empty edges
--        addGSVector = V.foldl' (\am (e, es) -> updateMap' e es am) M.empty genesets
--        addGSVector' = V.foldl' (\am (e, es) -> updateMap'' e es am) M.empty genesets
--        updateMap :: Entity -> Entity -> (Map Int (Set Int)) -> Map Int (Set Int)
--        updateMap e1 e2 m = M.insertWith (\old new -> S.union old new) (edex e1) (sing e2) m
--        updateMap' e es m = M.insertWith (\old new -> S.union old new) (edex e) (slist es) m
--        updateMap'' e es m = V.foldl' (\am gene -> M.insertWith S.union (edex gene) (sing e) am) m es --M.insertWith (\old new -> S.union old new) (slist es) (edex e) m
--        -- shouldn't be any -1 indices, doing this is bad
--        edex :: Entity -> Int
--        edex x = M.findWithDefault (-1) x em
--        sing :: Entity -> Set Int
--        sing = S.singleton . edex
--        slist = S.fromList . fmap edex . V.toList

buildDirectedGraph :: Map Entity Int -> Vector (Entity, Entity) 
                   -> Map Int (Set Int) -> Map Int (Set Int)
--
buildDirectedGraph em ve g = 
    V.foldl' (\g' (e1, e2) -> updateGraph e1 e2 g') g ve

    where
        updateGraph e1 e2 = 
            M.insertWith (\o n -> S.union o n) (dex e1) (set e2)
        dex x = M.findWithDefault (-1) x em
        set = S.singleton . dex

buildDirectedGraph' :: Map Entity Int -> Vector (Entity, Vector Entity) 
                    -> Map Int (Set Int) -> Map Int (Set Int)
--
buildDirectedGraph' em ve g = 
    V.foldl' (\g' (e, es) -> updateGraph e es g') g ve
    where
        updateGraph e es = 
            M.insertWith (\o n -> S.union o n) (dex e) (set es)
        dex x = M.findWithDefault (-1) x em
        set = S.fromList . fmap dex . V.toList

-- | Builds an undirected Entity graph by adding the list of 1:1 Entity
-- | relationships to a given graph. The entities are indexed prior to their
-- | addition to the graph.
--
buildUndirectedGraph :: Map Entity Int -> Vector (Entity, Entity) 
                     -> Map Int (Set Int) -> Map Int (Set Int)
--
buildUndirectedGraph em ve g = buildDirectedGraph em (fmap swap ve) $ 
                               buildDirectedGraph em ve g
    -- V.foldl' (\g' (e1, e2) -> updateGraph e2 e1 $ updateGraph e1 e2 g') g ve
    -- where
    --     updateGraph e1 e2 = 
    --         M.insertWith (\o n -> S.union o n) (dex e1) (set e2)
    --     dex x = M.findWithDefault (-1) x em
    --     set = S.singleton . dex

-- | Exactly like the buildUndirectedGraph function but uses a list of 1:many
-- | Entity relationships to a given graph.
--
buildUndirectedGraph' :: Map Entity Int -> Vector (Entity, Vector Entity) 
                      -> Map Int (Set Int) -> Map Int (Set Int)
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
-- | outgoing edges (outdegree == 0). Not to fear, dangling nodes are updated
-- | in the function below.
--
ensureGraphCompleteness :: Map Entity Int -> Map Int (Set Int) 
                        -> Map Int (Set Int)
-- 
ensureGraphCompleteness em g = M.foldl' (\g' x -> exists x g') g em
    where
        exists k g' = if M.member k g' then g' else M.insert k S.empty g'

-- | Updates dangling nodes, i.e. nodes that have an out-degree of zero. These
-- | nodes are updated to point to the special sink node.
--
updateDanglingNodes :: Int -> Map Int (Set Int) -> Map Int (Set Int)
--
updateDanglingNodes si g = M.foldlWithKey' (\g' k s -> ifEmpty k s g') g g
    where
        ifEmpty k s g' = if S.null s 
                         then M.insert k (S.singleton si) g'
                         else g'

-- | Permute graph labels for permutation testing.
--
permuteGraphLabels :: Map Entity Int -> IO (Map Entity Int)
--
permuteGraphLabels me = do
    keys <- shuffleM $ M.keys me
    vals <- shuffleM $ M.elems me

    return $ M.fromList $ zip keys vals

-- | Simulates noise by adding a percentage of false edges to the graph.
--
simulateNoise :: Float -> Map Int (Set Int) -> IO (Map Int (Set Int))
--
simulateNoise noise g
    | noise == 0 = return g
    | noise < 0 = return g
    | otherwise = do
        edges <- randomEdges

        return $ foldl' (\g' (e1, e2) -> M.insertWith S.union e1 (set e2) g') g edges
    where
        fi = fromIntegral
        set = S.singleton
        --
        -- Returns the number of edges present in the graph
        --
        numEdges = M.foldl' (\ac s -> ac + S.size s) 0 g
        numNodes = M.size g
        --
        -- Returns the number of edges that should be removed from the graph,
        -- based on the graph size and % noise parameter
        --
        numNoise = floor $ (fi numEdges) * (noise / 100.00)
        --
        -- Concatenates the list of nodes with itself until it is larger than
        -- the n parameter which represents the number of edges to add.
        -- Since we randomly select a node in the graph, then add a random
        -- edge to it, there could be a case where the # of edges to add >
        -- than the # of nodes in the graph. This function could be optimized
        -- but idc.
        --
        padNodeList n ns = if length ns >= n then ns else padNodeList n (ns ++ ns)
        --
        -- Generates a list of random nodes which we create edges to.
        --
        --randomNodes1 = take numNoise <$> (shuffleM $ padNodeList numNoise $ M.keys g)
        --randomNodes2 = take numNoise <$> (shuffleM $ padNodeList numNoise $ M.keys g)
        --randomEdges = zip <$> randomNodes1 <*> randomNodes2
        randomEdges = zip <$> randomNodes <*> randomNodes
        randomNodes = randomNodes' numNodes numNodes
        randomNodes' :: Int -> Int -> IO [Int]
        randomNodes' 0 _ = return []
        randomNodes' n s = do
            rn <- randomRIO (0, s - 1)
            rs <- randomNodes' (n - 1) s

            return (rn : rs)

-- | Simulates sparsity by removing a percentage of edges from the graph.
--
simulateSparsity :: Float -> Map Int (Set Int) -> IO (Map Int (Set Int))
--
simulateSparsity miss g
    | miss == 0 = return g
    | miss < 0 = return g
    | miss >= 100 = return g
    | otherwise = do
        nodes <- randomNodes

        --return $ foldl' (\g' n -> M.adjust removeSetElement n g') g nodes
        return $ foldl' (\g' (n, ei) -> M.adjust (removeSetElement ei) n g') g nodes
    where
        fi = fromIntegral
        --
        -- Returns the number of edges present in the graph
        --
        numEdges = M.foldl' (\ac s -> ac + S.size s) 0 g
        --
        -- Returns the number of edges that should be removed from the graph,
        -- based on the graph size and % missing parameter
        --
        numMiss = floor $ (fi numEdges) * (miss / 100.0)
        --
        -- Concatenates the list of nodes with itself until it is larger than
        -- the m parameter which represents the number of edges to remove.
        -- Since we randomly select a node in the graph, then remove a random
        -- edge from it, there could be a case where the # of edges to remove >
        -- than the # of nodes in the graph. This function could be optimized
        -- but idc.
        --
        padNodeList m ns = if length ns >= m then ns else padNodeList m (ns ++ ns)
        --
        -- Enumerates all edges in the graph as a list of edge, index pairs 
        -- [(e, i)]. The index i corresponds to the location of a node in the
        -- list nodes with an edge from e.
        --
        enumerateEdges g' = concat $ 
                            fmap (\(k, v) -> zip (replicate v k) [0 .. v - 1]) $ 
                            fmap (\(k, vs) -> (k, S.size vs)) $ 
                            M.toList g'
        --
        -- Generates a list of random nodes which we remove a single edge from.
        --
        --randomNodes = take numMiss <$> (shuffleM $ padNodeList numMiss $ M.keys g)
        randomNodes = (take numMiss . sortOnIndex) <$> (shuffleM $ enumerateEdges g)
        sortOnIndex = sortBy (\(_, a) (_, b) -> compare b a)
        removeIndex i xs = let (a, b) = splitAt i xs in a ++ drop 1 b
        --removeSetElement = S.fromList . drop 1 . S.toList
        removeSetElement i = S.fromList . removeIndex i . S.toList

-- | Simulates sparsity by removing a percentage of edges from the graph.
--
simulateSparsity' :: Float -> Map Int (Set Int) -> IO (Map Int (Set Int))
--
simulateSparsity' miss g
    | miss == 0 = return g
    | miss < 0 = return g
    -- | miss >= 100 = return g
    | otherwise = do
        --nodes <- randomNodes
        nodes <- randomEdges

        --return $ foldl' (\g' n -> M.adjust removeSetElement n g') g nodes
        --return $ foldl' (\g' (n, ei) -> M.adjust (removeSetElement ei) n g') g nodes
        return $ foldl' (\g' (n, ei) -> M.adjust (S.delete n) ei $ M.adjust (S.delete ei) n g') g nodes
    where
        fi = fromIntegral
        --
        -- Returns the number of edges present in the graph
        --
        numEdges = M.foldl' (\ac s -> ac + S.size s) 0 g
        --
        -- Returns the number of edges that should be removed from the graph,
        -- based on the graph size and % missing parameter
        --
        --numMiss = floor $ (fi numEdges) * (miss / 100.0)
        -- Divided by two because each undirected edge is essential two edges
        -- to -> from and from -> to.
        --numMiss = floor (miss / 2)
        numMiss = floor (((fi numEdges) * (miss / 100.0)) / 2)
        --
        -- Concatenates the list of nodes with itself until it is larger than
        -- the m parameter which represents the number of edges to remove.
        -- Since we randomly select a node in the graph, then remove a random
        -- edge from it, there could be a case where the # of edges to remove >
        -- than the # of nodes in the graph. This function could be optimized
        -- but idc.
        --
        padNodeList m ns = if length ns >= m then ns else padNodeList m (ns ++ ns)
        --
        -- Enumerates all edges in the graph as a list of edge, index pairs 
        -- [(e, i)]. The index i corresponds to the location of a node in the
        -- list nodes with an edge from e.
        --
        --enumerateEdges g' = concat $ 
        --                    fmap (\(k, v) -> zip (replicate v k) [0 .. v - 1]) $ 
        --                    fmap (\(k, vs) -> (k, S.size vs)) $ 
        --                    M.toList g'
        enumerateEdges' g' = concat $ 
                            --fmap (\(k, v) -> zip (replicate v k) [0 .. v - 1]) $ 
                            --fmap (\(k, vs) -> (k, S.size vs)) $ 
                            foldl' (\ac (k, vs) -> fmap (k,) (S.toList vs) : ac) [] $
                            M.toList g'
        --
        -- Generates a list of random nodes which we remove a single edge from.
        --
        --randomNodes = take numMiss <$> (shuffleM $ padNodeList numMiss $ M.keys g)
        --randomNodes = (take numMiss . sortOnIndex) <$> (shuffleM $ enumerateEdges g)
        randomEdges = (take numMiss) <$> (shuffleM $ enumerateEdges' g)
        --sortOnIndex = sortBy (\(_, a) (_, b) -> compare b a)
        removeIndex i xs = let (a, b) = splitAt i xs in a ++ drop 1 b
        --removeSetElement = S.fromList . drop 1 . S.toList
        removeSetElement i = S.fromList . removeIndex i . S.toAscList

