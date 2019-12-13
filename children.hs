module Children where

import Data.Matrix
import System.Random
import Control.Monad
import Control.Monad.Trans.State

import Utils
import Vars
import Organism


-- Generate a random double and pass on the next state of the random generator
random_double :: State StdGen Double
random_double = do
    gen <- get
    let (d, new_gen) = randomR (0,1) gen
    put new_gen
    return d

-- Generate a list of n random doubles
random_doubles :: Int -> State StdGen [Double]
random_doubles n = replicateM n random_double

-- Generate a random matrix of doubles, adjusted by the corresponding fitness matrix
adj_doubles_matrix :: RealFrac a => Matrix a -> State StdGen (Matrix Double)
adj_doubles_matrix m = do
    gen <- get
    let (lis, gen') = runState (random_doubles (num_signals * num_objects)) gen
    let mat = fromList num_signals num_objects lis
    put gen'
    return $ zipMatrices (\x fit -> x / (1-fit)) mat (fmap realToFrac m)


-- Create a child from an organism and a seed matrix
make_child :: Matrix Double -> Organism -> Organism
make_child m org = Organism new_matrix
    where
        new_matrix = AssociationMatrix $ zipMatrices flip_val m (getOrgMatrix org)
        flip_val seed orig = if orig == 1 then flip_to_not seed else flip_to_yes seed
        flip_to_not seed = if seed < err_rem then 0 else 1
        flip_to_yes seed = if seed < err_add then 1 else 0

-- Generate a random child organism
gen_child :: RealFrac a => Matrix a -> Organism -> State StdGen Organism
gen_child m parent = do
    gen <- get
    let (seed, gen') = runState (adj_doubles_matrix m) gen
    let child = make_child seed parent
    put gen'
    return child

-- Generate a list of n random children
gen_children :: RealFrac a => Matrix a -> Int -> Organism -> State StdGen [Organism]
gen_children m n parent = replicateM n (gen_child m parent)

-- Generate a list of children based on some fitness
gen_children_fit :: RealFrac a => Matrix a -> a -> Organism -> State StdGen [Organism]
gen_children_fit m f parent = do
    gen <- get
    let max_children = reproduction_rate * (realToFrac f)
    let (num_children, gen') = randomR (0, max_children+1) gen
    let (children, gen'') = runState (gen_children m (truncate num_children) parent) gen'
    put gen''
    return children
