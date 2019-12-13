module Organism where

import Data.Matrix
import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Data.List
import Vars
import Utils
import Objects


-- type Association = Integer
type Association = Rational

assocToRational :: Association -> Rational
assocToRational = id
-- assocToRational = realToFrac
assocToFractional  = fromRational . assocToRational



-- A matrix of associations between signals and objects
newtype AssociationMatrix = AssociationMatrix { getMatrix :: Matrix Association }
    deriving (Show, Eq)

-- Helper function to construct an association matrix from a function of the row and column
matrix_a :: ((Int, Int) -> Association) -> AssociationMatrix
matrix_a = AssociationMatrix . matrix num_signals num_objects

-- Helper function to construct an association matrix from a list of values
fromList_a :: [Association] -> AssociationMatrix
fromList_a = AssociationMatrix . fromList num_signals num_objects

-- Upper and lower bounds of an association matrix are all 0s and all 1s
instance Bounded AssociationMatrix where
    minBound = matrix_a $ \(i,j) -> 0
    maxBound = matrix_a $ \(i,j) -> 1

-- instance Random AssociationMatrix where
--     randomR (a, b) g =
--         case randomR (fromEnum a, fromEnum b) g of
--             (o, g') -> (toEnum o, g')
--     random g = randomR (minBound, maxBound) g




-- An organism, represented by an association matrix between signals and objects
data Organism = Organism {
    association :: AssociationMatrix
} deriving (Eq)

-- Method to print an organism
instance Show Organism where
    show o = "Organism:\n" ++ (show $ getOrgMatrix o) ++ "\n"

-- Helper function to get the association matrix of an organism
getOrgMatrix :: Organism -> Matrix Association
getOrgMatrix = getMatrix . association

-- In the paper, this is denoted with P
-- Probability that the given organism will use signal j for object i
send_matrix :: Organism -> Matrix Rational
send_matrix = normalizeRows . fmap assocToRational . getOrgMatrix

-- In the paper, this is denoted with Q
-- Probability that the given organism will interpreting signal j as refering to object i
hear_matrix :: Organism -> Matrix Rational
hear_matrix = normalizeCols . fmap assocToRational . getOrgMatrix


-- Probability of organism X succesfully communicating "i" to organism Y
-- This consists of three parts:
--      The probability that X will use some signal j to refer to the object i
--      The probability that Y will hear signal j as signal k (based on similarity of signals)
--      The probability that Y will interpret signal k to mean the object i
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



-- A population of organisms
type Population = [Organism]

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
