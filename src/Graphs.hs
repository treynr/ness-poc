
-- | file: Graphs.hs
-- | desc: Graph functions.
-- | vers: 0.1.0
-- | auth: TR

module Graphs where

import Algebra.Graph
import Data.ByteString.Char8 (ByteString)
import Data.List (foldl')
import Data.Set (Set)
import Data.IntMap.Strict (IntMap)
import Data.Map.Strict (Map)

import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import Types

makeGeneGraph :: [(Gene, Gene)] -> Graph EntityNode
--
makeGeneGraph = edges . foldl' (\ac (a, b) -> en (a, b) : en (b, a) : ac) []
    where
        en (a, b) = (GeneNode a, GeneNode b)

makeGeneSetGraph :: [(GeneSet, [Gene])] -> Graph EntityNode
--
makeGeneSetGraph = overlays . foldl' (\ac t -> assocs t : assocs' t : ac) []
    where
        assocs (s, gs) = edges [(GeneSetNode s, GeneNode g') | g' <- gs]
        assocs' (s, gs) = edges [(GeneNode g', GeneSetNode s) | g' <- gs]

makeEntityGraph :: [(Gene, Gene)] -> [(GeneSet, [Gene])] -> Graph EntityNode
--
makeEntityGraph gs sets = overlay (makeGeneGraph gs) (makeGeneSetGraph sets)

removeDuplicates :: Ord a => [a] -> [a]
--
removeDuplicates = S.toList . S.fromList


{-
makeGeneSetGraph' :: [(GeneSet, [Gene])] -> Graph EntityNode
--
makeGeneSetGraph' = overlays . foldl' (\ac t -> assocs t : assocs' t : ac) []
    where
        assocs (s, gs) = edges [(GeneSetNode s, GeneNode g') | g' <- gs]
        assocs' (s, gs) = edges [(GeneNode g', GeneSetNode s) | g' <- gs]

makeEntityGraph' :: [(Gene, Gene)] -> [(GeneSet, [Gene])] -> Graph EntityNode
--
makeEntityGraph' gs sets = overlay (makeGeneGraph gs) (makeGeneSetGraph sets)

makeGeneGraph' :: Int -> [(Gene, Gene)] -> (Int, Graph EntityNode)
--
makeGeneGraph' n = edges . foldl' (\ac (a, b) -> en (a, b) : en (b, a) : ac) []
    where
        en (a, b) = (GeneNode a, GeneNode b)

tagEntities :: [(Gene, Gene)] -> [(GeneSet, [Gene])] -> IntMap EntityNode'
--
tagEntities gg gsg = IM.union setMap geneMap
    where
        setMap = foldl' (\ac n@(GeneSetNode' gid x) -> IM.insert gid n ac) geneMap tagSets
        geneMap = foldl' (\ac n@(GeneNode' gid x) -> IM.insert gid n ac) IM.empty tagGenes
        tagSets = map (\(gid, x) -> GeneSetNode' gid x) $ zip setIds uniqueSets
        tagGenes = map (\(gid, x) -> GeneNode' gid x) $ zip geneIds uniqueGenes
        setIds = [geneLen .. ((geneLen + setLen) - 1)]
        geneIds = [0 .. (geneLen - 1)]
        geneLen = length uniqueGenes
        setLen = length uniqueSets
        uniqueGenes = removeDuplicates (genes ++ genes')
        uniqueSets = removeDuplicates sets
        -- Returns all the genes in the edge list
        genes = foldl' (\ac (a, b) -> a : b : ac) [] gg
        -- Returns all the genes associated to all sets as a single list
        genes' = concatMap snd gsg
        -- Returns all sets
        sets = map fst gsg
-}

tagEntities :: [(Gene, Gene)] -> [(GeneSet, [Gene])] -> [Term] -> IntMap EntityNode
--
tagEntities gg gsg ts = IM.union setMap geneMap
    where
        setMap = foldl' (\ac (gid, x) -> IM.insert gid (GeneSetNode x) ac) IM.empty tagSets
        geneMap = foldl' (\ac (gid, x) -> IM.insert gid (GeneNode x) ac) IM.empty tagGenes
        termMap = foldl' (\ac (gid, x) -> IM.insert gid (TermNode x) ac) IM.empty tagTerms
        tagGenes = zip geneIds uniqueGenes
        tagSets = zip setIds uniqueSets
        tagTerms = zip setIds uniqueTerms
        termIds = [(geneLen + setLen) .. ((geneLen + setLen + termLen) - 1)]
        setIds = [geneLen .. ((geneLen + setLen) - 1)]
        geneIds = [0 .. (geneLen - 1)]
        geneLen = length uniqueGenes
        setLen = length uniqueSets
        termLen = length uniqueTerms
        uniqueGenes = removeDuplicates (genes ++ genes')
        uniqueSets = removeDuplicates sets
        uniqueTerms = removeDuplicates ts
        -- Returns all the genes in the edge list
        genes = foldl' (\ac (a, b) -> a : b : ac) [] gg
        -- Returns all the genes associated to all sets as a single list
        genes' = concatMap snd gsg
        -- Returns all sets
        sets = map fst gsg

nodeToIdMap :: IntMap EntityNode -> Map EntityNode Int
--
nodeToIdMap im = M.fromList $ map (\(a, b) -> (b, a)) $ IM.toList im

