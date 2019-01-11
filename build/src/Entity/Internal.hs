
-- | file: Entity.Internal.hs
-- | desc: Internal functions used by the public Entity module. 
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}

module Entity.Internal (

    (<+>)
  , removeDuplicates
  , removeDuplicates'

) where

import Data.Vector           (Vector)

import qualified Data.Set              as S
import qualified Data.Vector           as V

-- | Utility function for vector concatenation
--
(<+>) :: Vector a -> Vector a -> Vector a
--
(<+>) = (V.++)

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

