
-- | file: WalkFFI.hs
-- | desc: Random Walk with Restart (RWR) implementation.
-- |       The bulk of the code is written in C and interfaced with Haskell via
-- |       the FFI. Use this implementation when dealing with huge graphs and 
-- |       memory is a concern. Keep in mind this implementation stil uses
-- |       large amounts of memory (~115GB for a graph with 90K nodes and 1
-- |       million edges) but substantially less than the immutable and linear
-- |       algebra implementations. 
-- | vers: 0.1.0
-- | auth: TR

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module WalkFFI where

import Control.Monad        (liftM)
import Data.List            (intercalate)
import Data.Vector.Storable (Vector, (!))
import Foreign
import Foreign.C.Types
import Numeric
import System.IO.Unsafe     (unsafePerformIO)

import qualified Data.Vector.Storable as V

foreign import ccall "walk.h randomWalkVector"
    c_randomWalkVector :: CInt -> CInt -> Ptr CInt -> Ptr CDouble -> CDouble ->
                          IO (Ptr CDouble)

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
randomWalk :: Int -> Int -> Vector Int -> Vector Double -> Double -> V.Vector Double
--
randomWalk n seedSize seed vs a = unsafePerformIO $ do
    let (fpvs, _, _) = V.unsafeToForeignPtr vs
    let (fps, _, _) = V.unsafeToForeignPtr seed

    pWalk <- liftM castPtr $ withForeignPtr fpvs $ \ptrvs -> withForeignPtr fps $ \ptrs ->
             c_randomWalkVector (fromIntegral n)
                                (fromIntegral seedSize) (castPtr ptrs)
                                (castPtr ptrvs) (realToFrac a) 

    fpWalk <- newForeignPtr finalizerFree pWalk

    return $ V.unsafeFromForeignPtr0 fpWalk n

-- | According to profiling this function eats up the most time/space,
-- | specifically colSums and normalize.
-- | Updated so that colSum results are memoized. Shaves off 3 minutes from run
-- | time.
--
normalize1DMatrix :: Int -> Vector Double -> Vector Double
--
normalize1DMatrix s vs = V.concat normalize
                         
    where
        velems = V.fromList [0 .. (s - 1)]
        rowSlice r = V.slice (s * r) s vs
        colSlice c = V.map (\r -> V.slice (s * r + c) 1 vs ! 0) velems
        colSum c = V.foldl' (+) 0.0 $ colSlice c 
        colSums = V.map colSum velems
        normalizeRow = V.imap (\i v -> if (colSums ! i) == 0.0 then 0.0 else v / (colSums ! i))
        normalize = fmap (normalizeRow . rowSlice) [0 .. (s - 1)]

print1DMatrix :: Int -> Vector Double -> IO ()
--
print1DMatrix s vs = putStrLn $ rowMap [0 .. (s - 1)]
    where
        --rowSlice r = fmap show $ V.toList $ V.slice (s * r) s vs
        rowSlice r = fmap (\f -> f "") $ fmap (showFFloat (Just 2)) $ V.toList $ V.slice (s * r) s vs
        rowMap = intercalate "\n" . fmap (intercalate "\t" . rowSlice)

