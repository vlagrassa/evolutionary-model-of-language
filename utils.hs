module Utils where

import Data.Matrix


mapIndex :: (Int -> Int -> a -> b) -> Matrix a -> Matrix b
mapIndex f m = matrix (nrows m) (ncols m) $ \(i,j) -> f i j (m ! (i,j))

normalizeRows :: RealFrac a => Matrix a -> Matrix a
normalizeRows m = mapIndex normalizeRow m
    where
        normalizeRow i _ n = if total i == 0 then 0 else n / (total i)
        total i = sum . getRow i $ m

normalizeCols :: RealFrac a => Matrix a -> Matrix a
normalizeCols m = mapIndex normalizeCol m
    where
        normalizeCol _ j n = if total j == 0 then 0 else n / (total j)
        total j = sum . getCol j $ m

normalizeRowsCols :: RealFrac a => Matrix a -> Matrix a
normalizeRowsCols m = mapIndex normalizeRowCol m
    where
        normalizeRowCol i j n = if total i j == 0 then 0 else n / (total i j)
        total i j = (sum . getRow i $ m) + (sum . getCol j $ m) - (m ! (i,j))

zipMatrices :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipMatrices f a b = matrix num_rows num_cols $ \(i,j) -> f (a ! (i,j)) (b !(i,j))
    where
        num_rows = min (nrows a) (nrows b)
        num_cols = min (ncols a) (ncols b)

avgMatrix :: [Matrix Rational] -> Matrix Rational
avgMatrix list = fmap (flip (/) len) . foldl1 (zipMatrices (+)) $ list
    where
        len = toRational $ length list


-- Round a number f to the nth place
roundN :: (RealFrac a1, Integral b, Fractional a2) => b -> a1 -> a2
roundN n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
