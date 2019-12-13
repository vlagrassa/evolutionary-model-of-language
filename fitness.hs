module Fitness where

import Utils
import Vars
import Objects
import Organism


-- Payoff values normalized such that 1.0 is the maximum possible payoff
norm_payoff :: Fractional a => Rational -> a
norm_payoff = (flip (/)) maxPayoff . fromRational
    where
        maxPayoff = realToFrac $ min num_signals num_objects

normalize_f :: Fractional c => (a -> b -> Rational) -> a -> b -> c
normalize_f f = curry $ norm_payoff . uncurry f


-- In the paper, this is denoted with F (Adapts equations 4 and 8)
-- The average payoff of either organism succesfully communicating an arbitrary message to the other
payoff :: Organism -> Organism -> Rational
payoff a b = toRational $ ((total_payoff a b) + (total_payoff b a)) / 2
    where
        -- Total payoff for X succesfully communicating with Y
        total_payoff x y = sum [(freq $ objects !! (i-1)) * success_payoff i x y | i <- [1..num_objects]]

        -- Payoff for X succesfully communicating "i" to Y
        success_payoff i x y = (id_payoff $ objects !! (i-1)) * (fromRational $ comm_success i x y)

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


-- Get the most fit organism from a population
fittest :: Population -> (Organism, Rational)
fittest pop = foldl1 max_fitness $ zip pop (fitness_arr_n pop)
    where
        max_fitness (a1, a2) (b1, b2) = if a2 > b2 then (a1,a2) else (b1,b2)
