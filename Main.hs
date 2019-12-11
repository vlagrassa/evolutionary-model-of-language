import System.Environment
import Data.Matrix
import System.Random
import Data.List

import Organism
import Vars
import Utils


type Population = [Organism]


payoff_percent :: Fractional a => Organism -> Organism -> a
payoff_percent = curry $ normalize_payoff . uncurry payoff

normalize_payoff :: Fractional a => Rational -> a
normalize_payoff = (flip (/)) maxPayoff . fromRational
    where
        maxPayoff = realToFrac $ min num_signals num_objects


-- The average payoff for an organism within a population
fitness :: Population -> Int -> Rational
fitness pop i = sum payoffs / (realToFrac $ length pop - 1)
    where
        payoffs = [(payoff (pop !! i) (pop !! x)) | x <- indices , x /= i]
        indices = [0 .. length pop - 1]


-- An array of the fitness of each organism in the population
fitness_arr :: Population -> [Rational]
fitness_arr pop = fmap (fitness pop) [0 .. length pop - 1]
-- (flip fmap) [0 .. length pop - 1] . avg_payoff_org


-- The average fitness of the population
avg_fitness :: Population -> Rational
avg_fitness pop = sum . fmap (flip (/) (toRational . length $ arr)) $ arr
    where
        arr = fitness_arr pop


-- A list of all the unique association matrices in the population
association_list :: Population -> [AssociationMatrix]
association_list = nub . fmap association


-- Frequency of individuals with a given association matrix in a given population
freq_association :: AssociationMatrix -> Population -> Rational
freq_association a pop = (num_matches a pop) / (realToFrac $ length pop)
    where
        num_matches a = sum . fmap (\x -> if association x == a then 1 else 0)

