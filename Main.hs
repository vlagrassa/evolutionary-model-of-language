import System.Environment
import Data.Matrix
import System.Random
import Data.List

import Organism
import Vars
import Utils


type Population = [Organism]


-- In the paper, this is denoted with P
-- Probability of using signal j for object i
send_matrix :: Organism -> Matrix Rational
send_matrix = normalizeRows . fmap assocToRational . getOrgMatrix

-- In the paper, this is denoted with Q
-- Probability of interpreting signal j as refering to object i
hear_matrix :: Organism -> Matrix Rational
hear_matrix = normalizeCols . fmap assocToRational . getOrgMatrix


-- In the paper, this is denoted with F (Adapts equations 4 and 8)
-- The average payoff of either organism succesfully communicating an arbitrary message to the other
payoff :: Organism -> Organism -> Rational
payoff a b = ((total_payoff a b) + (total_payoff b a)) / 2
    where
        -- Total payoff for X succesfully communicating with Y
        total_payoff x y = sum [success_payoff i x y | i <- [1..num_objects]]

        -- Payoff for X succesfully communicating "i" to Y
        success_payoff i x y = (comm_payoff x !! (i-1)) * (comm_success i x y)


-- Probability of X succesfully communicating "i" to Y
comm_success :: Int -> Organism -> Organism -> Rational
comm_success i x y = sum success_array
    where
        -- Probability of successful communication using each signal
        success_array = [prob_success j k | j <- [1..num_signals], k <- [1..num_signals]]

        -- Probability of successful communication where X sends j and Y hears k
        prob_success j k = (prob_send j) * (prob_error j k) * (prob_hear k)

        -- Probability that the sender will denote object "i" with signal "j"
        prob_send j = send_matrix x ! (i,j)

        -- Probability that listener will hear signal "j" as signal "k"
        prob_error j k = toRational $ error_matrix ! (j,k)

        -- Probability that listener will infer object "i" from signal "k"
        prob_hear k = hear_matrix y ! (i,k)


payoff_percent :: Fractional a => Organism -> Organism -> a
payoff_percent = curry $ normalize_payoff . uncurry payoff

normalize_payoff :: Fractional a => Rational -> a
normalize_payoff = (flip (/)) maxPayoff . fromRational
    where
        maxPayoff = realToFrac $ min num_signals num_objects


-- The average payoff for a population
avg_fitness :: Population -> Rational
avg_fitness pop = sum payoffs / weight
    where
        -- List of all payoffs
        payoffs = fmap (uncurry payoff) organisms

        -- List of all permuations of two organisms
        organisms = fmap (\(x,y) -> (pop !! x, pop !! y)) indices

        -- List of all permutations of two indices in the population
        indices = [(x,y) | x <- [0 .. length pop - 1], y <- [0 .. length pop - 1], x /= y]

        -- Total weight to divide by
        weight = realToFrac $ (length pop) * (length pop - 1)


-- The average payoff for an organism within a population
fitness :: Population -> Int -> Rational
fitness pop i = sum payoffs / (realToFrac $ length pop - 1)
    where
        payoffs = [(payoff (pop !! i) (pop !! x)) | x <- indices , x /= i]
        indices = [0 .. length pop - 1]


fitness_arr :: Population -> [Rational]
fitness_arr pop = fmap (fitness pop) [0 .. length pop - 1]
-- (flip fmap) [0 .. length pop - 1] . avg_payoff_org


-- A list of all the unique association matrices in the population
association_list :: Population -> [AssociationMatrix]
association_list = nub . fmap association


-- Frequency of individuals with a given association matrix in a given population
freq_association :: AssociationMatrix -> Population -> Rational
freq_association a pop = (num_matches a pop) / (realToFrac $ length pop)
    where
        num_matches a = sum . fmap (\x -> if association x == a then 1 else 0)

