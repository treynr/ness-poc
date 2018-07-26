
-- | file: Entity.hs
-- | desc: Entity type definitions, functions for manipulating Entity types, and
-- |       methods for preprocessing the constructed entity graph.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity where

import Control.DeepSeq       (NFData)
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict       (Map)
import Data.Vector           (Vector)
import GHC.Generics          (Generic)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.Vector           as V

import Utility ((<+>))

-- | The Gene type, which is simply a numeric gene ID. Normally this will be a
-- | non-permanent GeneWeaver ID.
--
data Gene = Gene !Int deriving (Show, Eq, Ord, Generic, NFData)

-- | The Homolog type, which is simply a numeric homolog ID. Normally this 
-- | will be a non-permanent GeneWeaver ID.
--
data Homolog = Homolog !Int deriving (Show, Eq, Ord, Generic, NFData)

-- | The GeneSet type. Each gene set is associated with a GSID, species ID, and
-- | some amount of metadata.
--
data GeneSet = GeneSet {

      gsid      :: !Int
    , species   :: !Int

} deriving (Show, Generic, NFData)

-- | The Term type. Represents an ontology term which has its own UID and short
-- | name.
--
data Term = Term {

      uid   :: !ByteString
    , name  :: !ByteString

} deriving (Show, Generic, NFData)

-- | The biological entity type. This polymorphic type can be a gene, homolog,
-- | gene set, ontology term, special sink node, or invalid.
--
data Entity = EGene Gene
            | EHomolog Homolog
            | EGeneSet GeneSet
            | ETerm Term
            | Sink
            | Invalid
            deriving (Show, Eq, Ord, Generic, NFData)

-- | GeneSet equivalence and order based soley on their GS IDs.
--
instance Eq GeneSet where
    (==) (GeneSet x _) (GeneSet y _) = x == y

instance Ord GeneSet where
    compare (GeneSet x _) (GeneSet y _) = compare x y

-- | Term equivalence and order based soley on their UIDs.
--
instance Eq Term where
    (==) (Term x _) (Term y _) = x == y

instance Ord Term where
    compare (Term x _) (Term y _) = compare x y

--
---- Helper functions for constructing Entity values.
--

buildGeneSetEntity :: Int -> Int -> Entity
--
buildGeneSetEntity gsid spid = EGeneSet $ GeneSet gsid spid

buildGeneEntity :: Int -> Entity
--
buildGeneEntity gid = EGene $ Gene gid

buildHomologEntity :: Int -> Entity
--
buildHomologEntity hid = EHomolog $ Homolog hid

buildTermEntity :: ByteString -> ByteString -> Entity
--
buildTermEntity tid tname = ETerm $ Term tid tname

buildSinkEntity :: Entity
--
buildSinkEntity = Sink

--
---- Helper functions for determining entity types.
--

isGene :: Entity -> Bool
--
isGene (EGene _) = True
isGene _ = False

isHomolog :: Entity -> Bool
--
isHomolog (EHomolog _) = True
isHomolog _ = False

isGeneSet :: Entity -> Bool
--
isGeneSet (EGeneSet _) = True
isGeneSet _ = False

isTerm :: Entity -> Bool
--
isTerm (ETerm _) = True
isTerm _ = False

isSink :: Entity -> Bool
--
isSink (Sink) = True
isSink _ = False

--
---- Helper functions for filtering lists of entities
--

ffilter :: (Applicative f, Foldable f, Monoid (f a)) => 
           (a -> Bool) -> f a -> f a
--
ffilter p = foldMap (\a -> if p a then pure a else mempty)

onlyTerms :: (Applicative f, Foldable f, Monoid (f Entity)) =>
             f Entity -> f Entity
--
onlyTerms = ffilter isTerm
    where
        isTerm (ETerm _) = True
        isTerm _ = False

onlyGeneSets ::  (Applicative f, Foldable f, Monoid (f Entity)) =>
                 f Entity -> f Entity
--
onlyGeneSets = ffilter isGS
    where
        isGS (EGeneSet _) = True
        isGS _ = False

onlyGenes :: (Applicative f, Foldable f, Monoid (f Entity)) =>
             f Entity -> f Entity
--
onlyGenes = ffilter isGene
    where
        isGene (EGene _) = True
        isGene _ = False

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
removeDuplicates' = V.fromList . removeDuplicates . V.toList

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


-- | Takes in a list of 1:1 Entity relationships, converts them into a 
-- | single list of entities with duplicates removed, and merges them with a 
-- | final Entity list. The result can then be used in the creation of the
-- | Entity graph.
--
flattenEntities :: Vector (Entity, Entity) -> Vector Entity -> Vector Entity
--
flattenEntities ve vf = removeDuplicates' $ ve' <+> vf
    where
        ve' = V.map fst ve <+> V.map snd ve

-- | Exactly like flatteEntities but uses a list of 1:many Entity
-- | relationships.
--
flattenEntities' :: Vector (Entity, Vector Entity) -> Vector Entity
                 -> Vector Entity
--
flattenEntities' ve vf = removeDuplicates' $ ve' <+> vf
    where
        ve' = V.map fst ve <+> V.concatMap snd ve

-- | Takes in entity relationships from all three input types (edge lists,
-- | genesets, and annotations) and converts them into a single list of
-- | entities with duplicates removed. The resulting list can then be used in 
-- | the creation of the Entity graph.
--
--flattenEntities :: Vector (Entity, Entity) -> Vector (Entity, Entity) 
--                -> Vector (Entity, Vector Entity) -> Vector (Entity, Entity) 
--                -> Vector (Entity, Entity) -> Vector Entity
--                   
----
--flattenEntities es hs gs as ts = removeDuplicates' $ es' <+> hs' <+> gs' <+> as' <+> ts'
--    where
--        es' = V.map fst es <+> V.map snd es
--        hs' = V.map fst hs <+> V.map snd hs
--        gs' = V.map fst gs <+> V.concat (V.toList $ V.map snd gs)
--        as' = V.map fst as <+> V.map snd as
--        ts' = V.map fst ts <+> V.map snd ts

-- | Serializes an Entity value into an appropriate bytestring for display or
-- | output.
--
serializeEntity :: Entity -> ByteString
--
serializeEntity (EGene (Gene g)) = B.pack $ ("GENE:" ++) $ show $ g
serializeEntity (EHomolog (Homolog h)) = B.pack $ ("HOM:" ++) $ show $ h
serializeEntity (EGeneSet g) = B.pack $ ("GS:" ++) $ show $ gsid g
serializeEntity (ETerm t) = uid t
serializeEntity (Invalid) = "Invalid Entity"
serializeEntity (Sink) = "Sink"

