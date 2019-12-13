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


-- The average association matrix of a population
avg_association :: Population -> Matrix Rational
avg_association pop = fmap (flip (/) len) . foldl1 (zipMatrices (+)) $ lis
    where
        lis = fmap (fmap assocToRational . getMatrix) . association_list $ pop
        len = toRational $ length lis


-- A metric of fitness for each possible association the organism can have
-- For some association between signal i and object j, this is the likelihood that
-- the signal i will be interpreted by the population as some object j, times the
-- total payoff for identifying j
assoc_fitness :: Population -> Matrix Rational
assoc_fitness pop = zipMatrices (*) (avg_association pop) payoff_matrix
    where
        -- The likelihood that i will be interpreted as j, times the payoff for identifying j, times the frequency of j
        payoff_matrix = fmap toRational $ matrix num_signals num_objects payoff_matrix_f

        -- Function to construct the above matrix
        payoff_matrix_f (i,j) = (total_payoff $ objects !! (j-1)) * (avg_understand_matrix ! (i,j))

        -- The average likelihood that i will be heard as referring to j across the population
        avg_understand_matrix = zipMatrices (*) error_matrix . fmap realToFrac . avgMatrix $ fmap hear_matrix pop




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

