
-- | file: Types.hs
-- | desc: Custom data types.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.ByteString.Char8 (ByteString)

data Gene = Gene {

    -- symbol :: ByteString
    ode :: Int

} deriving (Show, Eq, Ord)

data GeneSet = GeneSet {

      gsId      :: Int
    , species   :: Int

} deriving (Show)

data Term = Term {

      uid       :: ByteString
    , termName  :: ByteString

} deriving (Show)

data EntityNode = GeneNode Gene 
                | GeneSetNode GeneSet 
                | TermNode Term 
                deriving (Show, Eq, Ord)

data EntityNode' = GeneNode' Int Gene | GeneSetNode' Int GeneSet | TermNode' Int Term

instance Eq GeneSet where
    (==) (GeneSet x _) (GeneSet y _) = x == y

instance Eq Term where
    (==) (Term x _) (Term y _) = x == y

instance Ord GeneSet where
    compare (GeneSet x _) (GeneSet y _) = compare x y

instance Ord Term where
    compare (Term x _) (Term y _) = compare x y

--instance Eq EntityNode where
--    (==) (GeneNode x) (GeneNode y) = x == y
--    (==) (GeneSetNode x) (GeneSetNode y) = x == y
--    (==) (TermNode x) (TermNode y) = x == y
--    (==) _ _ = False

instance Eq EntityNode' where
    (==) (GeneNode' _ x) (GeneNode' _ y) = x == y
    (==) (GeneSetNode' _ x) (GeneSetNode' _ y) = x == y
    (==) (TermNode' _ x) (TermNode' _ y) = x == y
    (==) _ _ = False

instance Ord EntityNode' where
    compare (GeneNode' i _) (GeneNode' j _) = compare i j
    compare (GeneSetNode' i _) (GeneSetNode' j _) = compare i j
    compare (TermNode' i _) (TermNode' j _) = compare i j
    compare (GeneNode' i _) (GeneSetNode' j _) = compare i j
    compare (GeneNode' i _) (TermNode' j _) = compare i j
    compare (GeneSetNode' i _) (TermNode' j _) = compare i j
