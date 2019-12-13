import System.Environment
import Data.Matrix
import System.Random
import Data.List
import Control.Monad.Trans.State
import Control.Monad

import Organism
import Vars
import Utils
import Objects
import Fitness
import Children






data Generation = Generation {
    population :: Population,
    generator  :: StdGen
} deriving (Show)



-- Function to create a new population of organisms from the current population
create_next_generation :: Population -> State StdGen Population
create_next_generation pop = do
    gen <- get
    let (children, new_gen) = foldl fold_func ([], gen) parents
    put new_gen
    return $ take carrying_capacity children
    where
        parents = sortBy (\(_,a) (_,b) -> compare b a) $ zip pop (scaled_fitness_arr pop)

        assoc = assoc_fitness pop

        fold_func (lis, g) (org, fit) = (lis ++ new_lis, g') where
            (new_lis, g') = runState (gen_children_fit assoc fit org) g


-- Function to move to the next generation
next_generation :: State Generation Rational
next_generation = do
    Generation pop gen <- get
    if length pop == 0
        then do
            put $ Generation pop gen
            return 0
        else do
            let fit = fitness_arr pop
            let (children, new_gen) = runState (create_next_generation pop) gen
            put $ Generation children new_gen
            return $ avg_fitness_n children


run_n_generations :: Int -> State Generation [Rational]
run_n_generations n = replicateM n next_generation

