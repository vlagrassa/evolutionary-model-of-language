module Vars where

import Data.Matrix
import Utils
import Signals

-- The number of signals
-- In the paper, this is denoted with n
num_signals :: Int
num_signals = 5

-- The number of messages
-- In the paper, this is denoted with m
num_objects :: Int
num_objects = 5

signals :: [Vowel]
signals = [v1, v2, v3, v4, v5]


-- Matrix of similarities between different signals
simil_matrix :: Matrix Double
simil_matrix = matrix num_signals num_signals $
    \(i,j) -> (signals !! (i-1)) <~> (signals !! (j-1))

-- Probabilities of confusing one signal for another
-- Normalized version of similarity matrix
error_matrix :: Matrix Double
error_matrix = normalizeRows simil_matrix

