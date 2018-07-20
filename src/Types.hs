
-- | file: Types.hs
-- | desc: Custom data types.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Types (
    Annotation,
    Entity(..),
    Gene(..),
    Homolog(..),
    GeneSet(..),
    Term(..),
    isEGene,
    isEHomolog,
    isEGeneSet,
    isETerm,
    isSink
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.ByteString.Char8 (ByteString)

data Gene = Gene {

    ode :: !Int

} deriving (Show, Eq, Ord, Generic, NFData)

data Homolog = Homolog {

    hom :: !Int

} deriving (Show, Eq, Ord, Generic, NFData)

data GeneSet = GeneSet {

      gsid      :: !Int
    , species   :: !Int

} deriving (Show, Generic, NFData)

data Term = Term {

      uid   :: !ByteString
    , name  :: !ByteString

} deriving (Show, Generic, NFData)

type Annotation = (ByteString, [Gene])

data Entity = EGene Gene
            | EHomolog Homolog
            | EGeneSet GeneSet
            | ETerm Term
            | Sink
            | Invalid
            deriving (Show, Eq, Ord, Generic, NFData)

-- GeneSet equivalence and order based soley on their gs_ids
--
instance Eq GeneSet where
    (==) (GeneSet x _) (GeneSet y _) = x == y

instance Ord GeneSet where
    compare (GeneSet x _) (GeneSet y _) = compare x y

-- Term equivalence and order based soley on their UIDs
--
instance Eq Term where
    (==) (Term x _) (Term y _) = x == y

instance Ord Term where
    compare (Term x _) (Term y _) = compare x y

-- Helper functions for determining entity types.
--


-- | Returns true if the given Entity is a Gene.
--
isEGene :: Entity -> Bool
--
isEGene (EGene _) = True
isEGene _ = False

-- | Returns true if the given Entity is a Homolog.
--
isEHomolog :: Entity -> Bool
--
isEHomolog (EHomolog _) = True
isEHomolog _ = False

-- | Returns true if the given Entity is a GeneSet.
--
isEGeneSet :: Entity -> Bool
--
isEGeneSet (EGeneSet _) = True
isEGeneSet _ = False

-- | Returns true if the given Entity is a Term.
--
isETerm :: Entity -> Bool
--
isETerm (ETerm _) = True
isETerm _ = False

-- | Returns true if the given Entity is the special Sink type.
--
isSink :: Entity -> Bool
--
isSink (Sink) = True
isSink _ = False

