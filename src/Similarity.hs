
-- | file: Similarity.hs
-- | desc: Functions related to calculating semantic similarity.
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE DuplicateRecordFields #-}

module Similarity where

import Data.ByteString.Char8 (ByteString)

data Gene = Gene {

    symbol :: ByteString

} deriving (Show, Eq)

data Geneset = Geneset {

      gsId      :: Int
    , gsName    :: ByteString
    , species   :: ByteString

} deriving (Show)

data Term = Term {

      uid       :: ByteString
    , termName  :: ByteString

} deriving (Show)

data EntityNode = GeneNode Gene | GenesetNode Geneset | TermNode Term

instance Eq Geneset where
    (==) (Geneset x _ _) (Geneset y _ _) = x == y

instance Eq Term where
    (==) (Term x _) (Term y _) = x == y

