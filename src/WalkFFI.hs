
-- | file: WalkFFI.hs
-- | desc: Random Walk with Restart (RWR) implementation.
-- |       The bulk of the code is written in C and interfaced with Haskell via
-- |       the FFI. Use this implementation when dealing with huge graphs and 
-- |       memory is a concern. Keep in mind this implementation stil uses
-- |       large amounts of memory (~250GB for a graph with 90K nodes and 1
-- |       million edges) but substantially less than the immutable and linear
-- |       algebra implementations. 
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module WalkFFI where

import Control.DeepSeq      (($!!))
import Control.Monad        (liftM)
import Data.List            (intercalate)
import Data.Map.Strict      (Map)
import Data.Vector.Storable (Vector, (!))
import Foreign
import Foreign.C.Types
import System.IO.Unsafe     (unsafePerformIO)

import qualified Data.Map               as M
import qualified Data.Vector.Storable   as V

foreign import ccall "walk.h randomWalkMatrix"
    c_randomWalkMatrix :: CInt -> CInt -> Ptr (Ptr CDouble) -> CDouble -> 
                          CDouble -> Ptr CDouble

foreign import ccall "walk.h randomWalkVector"
    c_randomWalkVector :: CInt -> CInt -> Ptr CDouble -> CDouble -> CDouble ->
                          IO (Ptr CDouble)

alpha = 0.15
alpha' = 1.0 - alpha


sample1d :: Vector Double
-- j -> i
sample1d = V.fromList [ 0.0, 1.0, 1.0, 0.0, 0.0, 0.0,
                        1.0, 0.0, 1.0, 1.0, 1.0, 1.0,
                        0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
                        1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                        0.0, 0.0, 0.0, 1.0, 0.0, 0.0,
                        1.0, 0.0, 0.0, 0.0, 0.0, 0.0 ]
sample1da :: Vector Double
-- 
sample1da = V.fromList [ 0.0, 1.0, 0.0, 1.0, 0.0, 1.0,
                         1.0, 0.0, 1.0, 1.0, 0.0, 0.0,
                         1.0, 1.0, 0.0, 0.0, 0.0, 0.0,
                         0.0, 1.0, 0.0, 0.0, 1.0, 0.0,
                         0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
                         0.0, 1.0, 0.0, 1.0, 0.0, 0.0 ]
sample1db :: Vector Double
-- 
sample1db = V.fromList [ 0.0, 1.0, 0.0, 1.0, 0.0, 1.0,
                         1.0, 0.0, 1.0, 1.0, 0.0, 0.0,
                         1.0, 1.0, 0.0, 0.0, 0.0, 0.0,
                         0.0, 1.0, 0.0, 0.0, 1.0, 0.0,
                         0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
                         0.0, 1.0, 0.0, 1.0, 0.0, 0.0 ]
-- | Haskell wrapper function for the random walk code in C.
--
randomWalk :: Int -> Int -> Vector Double -> Double -> Double -> V.Vector Double
--
randomWalk n seed vs a a' = unsafePerformIO $ do
    let (fpvs, _, lenvs) = V.unsafeToForeignPtr vs

    pWalk <- liftM castPtr $ withForeignPtr fpvs $ \ptrvs ->
             c_randomWalkVector (fromIntegral n) (fromIntegral seed) 
                                (castPtr ptrvs) (realToFrac alpha) 
                                (realToFrac alpha')

    fpWalk <- newForeignPtr finalizerFree pWalk

    return $ V.unsafeFromForeignPtr0 fpWalk n

normalize1DMatrix :: Int -> Vector Double -> Vector Double
--
normalize1DMatrix s vs = V.concat $!! columnMap [0 .. (s - 1)]
                         
    where
        colSlice r = V.slice (s * r) s vs
        colSum r = V.foldl' (+) 0.0 $ colSlice r 
        normalSlice s' vs' = if s' == 0.0 
                             then V.map (\_ -> 0.0) vs'
                             else V.map (\x -> x / s') vs'
        columnMap = fmap (\r -> normalSlice (colSum r) $!! colSlice r)

normalize1DMatrix' :: Int -> Vector Double -> Vector Double
--
normalize1DMatrix' s vs = V.concat normalize
                         
    where
        velems = V.fromList [0 .. (s - 1)]
        rowSlice r = V.slice (s * r) s vs
        colSlice c = V.map (\r -> (V.slice (s * r + c) 1 vs) ! 0) velems
        colSum c = V.foldl' (+) 0.0 $ colSlice c 
        colSums = V.map colSum velems
        normalizeRow cs vs = V.imap (\i v -> if (cs ! i) == 0.0 then 0.0 else v / (cs ! i)) vs
        normalize = fmap (\r -> normalizeRow colSums $ rowSlice r) [0 .. (s - 1)]

        --columnMap = fmap (\r -> normalSlice (colSum r) $!! colSlice r)

print1DMatrix :: Int -> Vector Double -> IO ()
--
print1DMatrix s vs = putStrLn $ rowMap [0 .. (s - 1)]
    where
        rowSlice r = fmap show $ V.toList $ V.slice (s * r) s vs
        rowMap = intercalate "\n" . fmap (\r -> intercalate "\t" $ rowSlice r)

