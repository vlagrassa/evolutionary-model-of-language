import System.Environment
import Data.Matrix
import System.Random
import Data.List
import Control.Monad.Trans.State

import Organism
import Vars
import Utils


type Population = [Organism]


-- Payoff values normalized such that 1.0 is the maximum possible payoff
norm_payoff :: Fractional a => Rational -> a
norm_payoff = (flip (/)) maxPayoff . fromRational
    where
        maxPayoff = realToFrac $ min num_signals num_objects

normalize_f :: Fractional c => (a -> b -> Rational) -> a -> b -> c
normalize_f f = curry $ norm_payoff . uncurry f


-- The payoff for successful communication between two organisms, normalized such that 1.0 is the maximum possible payoff
payoff_n :: Fractional a => Organism -> Organism -> a
payoff_n = normalize_f payoff


-- The average payoff for an organism within a population
fitness :: Population -> Int -> Rational
fitness pop i = sum payoffs / (realToFrac $ length pop - 1)
    where
        payoffs = [(payoff (pop !! i) (pop !! x)) | x <- indices , x /= i]
        indices = [0 .. length pop - 1]

fitness_n :: Fractional a => Population -> Int -> a
fitness_n = normalize_f fitness


-- An array of the fitness of each organism in the population
fitness_arr :: Population -> [Rational]
fitness_arr pop = fmap (fitness pop) [0 .. length pop - 1]

fitness_arr_n :: Fractional a => Population -> [a]
fitness_arr_n = fmap norm_payoff . fitness_arr


-- The average fitness of the population
avg_fitness :: Population -> Rational
avg_fitness pop = sum . fmap (flip (/) (toRational . length $ arr)) $ arr
    where
        arr = fitness_arr pop

avg_fitness_n :: Fractional a => Population -> a
avg_fitness_n = norm_payoff . avg_fitness


-- An array of the fitness of each organism in the population, curved by the highest fitness
scaled_fitness_arr :: Population -> [Rational]
scaled_fitness_arr pop = fmap (flip (/) max_value) $ fitness_arr pop
    where
        max_value = foldl1 max . fitness_arr $ pop



-- A list of all the unique association matrices in the population
association_list :: Population -> [AssociationMatrix]
association_list = nub . fmap association


-- Frequency of individuals with a given association matrix in a given population
freq_association :: AssociationMatrix -> Population -> Rational
freq_association a pop = (num_matches a pop) / (realToFrac $ length pop)
    where
        num_matches a = sum . fmap (\x -> if association x == a then 1 else 0)



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
    return children
    where
        parents = zip pop (scaled_fitness_arr pop)

        fold_func (lis, g) (org, fit) = (lis ++ new_lis, g') where
            (new_lis, g') = runState (gen_children_fit fit org) g


-- Function to move to the next generation
next_generation :: State Generation Rational
next_generation = do
    Generation pop gen <- get
    let fit = fitness_arr pop
    let (children, new_gen) = runState (create_next_generation pop) gen
    put $ Generation children new_gen
    return (avg_fitness children)
