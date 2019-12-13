module Vars where

import Data.Matrix
import Utils
import Signals
import Objects

-- The list of signals
signals :: [Syllable]
signals = [syl_1, syl_2, syl_3, syl_4, syl_5]

-- The number of signals
-- In the paper, this is denoted with n
num_signals :: Int
num_signals = length signals


objects :: [Object]
objects = [Object 1 1, Object 0.68 0.39, Object 0.83 0.9, Object 0.78 0.58, Object 0.91 1]

-- The number of messages
-- In the paper, this is denoted with m
num_objects :: Int
num_objects = length objects


-- Probability of child forming new association
-- In the paper, this is denoted with w_0
-- 0.005
err_add = 0.01

-- Probability of child losing parent's association
-- In the paper, this is denoted with w_1
-- 0.0001
err_rem = 0.0005


-- Maximum number of children the most fit organism can have
reproduction_rate = 3.0


-- Maximum size of the population that can be supported
carrying_capacity :: Int
carrying_capacity = 30


-- Matrix of similarities between different signals
simil_matrix :: Matrix Double
simil_matrix = matrix num_signals num_signals $
    \(i,j) -> (signals !! (i-1)) <~> (signals !! (j-1))

-- Probabilities of confusing one signal for another
-- Normalized version of similarity matrix
error_matrix :: Matrix Double
error_matrix = normalizeRows simil_matrix

