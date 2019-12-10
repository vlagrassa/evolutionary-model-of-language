module Vars where

import Data.Matrix
import Utils

-- The number of signals
-- In the paper, this is denoted with n
num_signals = 5

-- The number of messages
-- In the paper, this is denoted with m
num_objects = 5


-- Matrix of similarities between different signals
simil_matrix :: Matrix Rational
simil_matrix = matrix num_signals num_signals $
    \(i,j) -> (realToFrac 1) / (realToFrac . (+) 1 . abs $ i - j)

-- Probabilities of confusing one signal for another
-- Normalized version of similarity matrix
error_matrix :: Matrix Rational
error_matrix = normalizeRows simil_matrix