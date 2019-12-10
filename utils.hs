module Utils where

import Data.Matrix


mapIndex :: (Int -> Int -> a -> b) -> Matrix a -> Matrix b
mapIndex f m = matrix (nrows m) (ncols m) $ \(i,j) -> f i j (m ! (i,j))

normalizeRows :: Fractional a => Matrix a -> Matrix a
normalizeRows m = mapIndex normalizeRow m
    where
        normalizeRow i _ n = n / (sum . getRow i $ m)

normalizeCols :: Fractional a => Matrix a -> Matrix a
normalizeCols m = mapIndex normalizeCol m
    where
        normalizeCol _ j n = n / (sum . getCol j $ m)