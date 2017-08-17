
-- | file: Types.hs
-- | desc: Custom data types.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.ByteString.Char8 (ByteString)

data Gene = Gene {

    ode :: !Int

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
            | EGeneSet GeneSet
            | ETerm Term
            | Invalid
            deriving (Show, Eq, Ord, Generic, NFData)

data EntityNode = GeneNode Gene 
                | GeneSetNode GeneSet 
                | TermNode Term 
                deriving (Show, Eq, Ord, Generic, NFData)

data EntityNode' = GeneNode' Int Gene 
                 | GeneSetNode' Int GeneSet 
                 | TermNode' Int Term

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

