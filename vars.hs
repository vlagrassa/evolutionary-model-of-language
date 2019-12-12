module Vars where

import Data.Matrix
import Utils
import Signals

-- The list of signals
signals :: [Vowel]
signals = [v1, v2, v3, v4, v5]

-- The number of signals
-- In the paper, this is denoted with n
num_signals :: Int
num_signals = length signals

-- The number of messages
-- In the paper, this is denoted with m
num_objects :: Int
num_objects = 5


-- Probability of child forming new association
-- In the paper, this is denoted with w_0
-- 0.005
err_add = 0.1

-- Probability of child losing parent's association
-- In the paper, this is denoted with w_1
-- 0.0001
err_rem = 0.1


-- Maximum number of children the most fit organism can have
reproduction_rate = 3


-- Matrix of similarities between different signals
simil_matrix :: Matrix Double
simil_matrix = matrix num_signals num_signals $
    \(i,j) -> (signals !! (i-1)) <~> (signals !! (j-1))

-- Probabilities of confusing one signal for another
-- Normalized version of similarity matrix
error_matrix :: Matrix Double
error_matrix = normalizeRows simil_matrix

