
-- | file: Entity.hs
-- | desc: Functions for manipulating Entity types and preprocessing the
-- |       constructed entity graph.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity where

import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict  (Map)
import Data.Vector      (Vector)

import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import qualified Data.Vector        as V

import Types
import Utility

-- | Removes duplicates from the list by inserting them into a Set and then
-- | converting back to a list. O(n log n) runtime.
--
removeDuplicates :: Ord a => [a] -> [a]
--
removeDuplicates = S.toList . S.fromList

-- | Removes duplicates from the vector by inserting them into a Set and then
-- | converting back to vector. O(n log n) runtime.
--
removeDuplicates' :: Ord a => Vector a -> Vector a
--
removeDuplicates' = V.fromList . S.toList . S.fromList . V.toList

-- | Given a list of entities, this function marks them with unique integer IDs
-- | that are 0-indexed for use in an adjacency matrix and returns the mapping
-- | of Entities -> IDs.
--
tagEntities :: Vector Entity -> Map Entity Int
--
tagEntities es = M.fromList $! V.toList $! V.zip es $! 
                 V.iterateN (V.length es) (+1) 0

-- | Takes in genes, sets, and terms, removes duplicates from each list and
-- | converts them into Entity types.
--
convertEntities :: [Gene] -> [GeneSet] -> [Term] -> [Entity]
--
convertEntities gs gss ts = gs' ++ gss' ++ ts'
    where
        -- Maps each entity type to the actual Entity type
        gs' = fmap EGene $! removeDuplicates gs
        gss' = fmap EGeneSet $! removeDuplicates gss
        ts' = fmap ETerm $! removeDuplicates ts


-- | Takes in entity relationships from all three input types (edge lists,
-- | genesets, and annotations) and converts them into a single list of
-- | entities with duplicates removed. The resulting list can then be used in 
-- | the creation of the Entity graph.
--
flattenEntities :: Vector (Entity, Entity) -> Vector (Entity, Entity) 
                -> Vector (Entity, Vector Entity) -> Vector (Entity, Entity) 
                -> Vector (Entity, Entity) -> Vector Entity
                   
--
flattenEntities es hs gs as ts = removeDuplicates' $ es' <+> hs' <+> gs' <+> as' <+> ts'
    where
        es' = V.map fst es <+> V.map snd es
        hs' = V.map fst hs <+> V.map snd hs
        gs' = V.map fst gs <+> V.concat (V.toList $ V.map snd gs)
        as' = V.map fst as <+> V.map snd as
        ts' = V.map fst ts <+> V.map snd ts

-- | Constructs a Term Entity.
--
termEntity :: ByteString -> ByteString -> Entity
--
termEntity tid tname = ETerm $! Term tid tname

sinkEntity :: Entity
--
sinkEntity = ETerm $! Term "$SINK" "$SINK"

