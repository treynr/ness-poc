
-- | file: Entity.hs
-- | desc: Entity type definitions, functions for manipulating Entity types, and
-- |       methods for preprocessing the constructed entity graph.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Entity (

    Entity(..)
  , buildGeneEntity
  , buildGeneSetEntity
  , buildHomologEntity
  , buildSinkEntity
  , buildTermEntity
  , flattenEntities
  , flattenEntities'
  , serializeEntity
  , tagEntities

) where

import Control.DeepSeq       (NFData)
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict       (Map)
import Data.Vector           (Vector)
import GHC.Generics          (Generic)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import qualified Data.Vector           as V

import Entity.Internal ((<+>), removeDuplicates, removeDuplicates')

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

      gsid    :: !Int
    , species :: !Int

} deriving (Show, Generic, NFData)

-- | The Term type. Represents an ontology term which has its own UID and short
-- | name.
--
data Term = Term {

      uid  :: !ByteString
    , name :: !ByteString

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

-- | Helper functions for constructing Entity values.
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

-- | Given a list of entities, this function marks them with unique integer IDs
-- | that are 0-indexed for use in an adjacency matrix and returns the mapping
-- | of Entities -> IDs.
--
tagEntities :: Vector Entity -> Map Entity Int
--
tagEntities es = M.fromList $! V.toList $! V.zip es $! V.iterateN (V.length es) (+1) 0

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
flattenEntities' :: Vector (Entity, Vector Entity) -> Vector Entity -> Vector Entity
--
flattenEntities' ve vf = removeDuplicates' $ ve' <+> vf
    where
        ve' = V.map fst ve <+> V.concatMap snd ve

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

