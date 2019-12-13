module Organism where

import Data.Matrix
import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Vars
import Utils
import Objects


-- type Association = Integer
type Association = Rational

assocToRational :: Association -> Rational
assocToRational = id
-- assocToRational = realToFrac
assocToFractional  = fromRational . assocToRational



newtype AssociationMatrix = AssociationMatrix { getMatrix :: Matrix Association }
    deriving (Show, Eq)

matrix_a :: ((Int, Int) -> Association) -> AssociationMatrix
matrix_a = AssociationMatrix . matrix num_signals num_objects

fromList_a :: [Association] -> AssociationMatrix
fromList_a = AssociationMatrix . fromList num_signals num_objects



-- An organism, represented by an association matrix between signals and objects
data Organism = Organism {

    -- Association matrix of signals and objects
    association :: AssociationMatrix

} deriving (Eq)

instance Show Organism where
    show o = "Organism:\n" ++ (show $ getOrgMatrix o) ++ "\n"





instance Bounded AssociationMatrix where
    minBound = matrix_a $ \(i,j) -> 0
    maxBound = matrix_a $ \(i,j) -> 1


-- instance Random AssociationMatrix where
--     randomR (a, b) g =
--         case randomR (fromEnum a, fromEnum b) g of
--             (o, g') -> (toEnum o, g')
--     random g = randomR (minBound, maxBound) g


getOrgMatrix :: Organism -> Matrix Association
getOrgMatrix = getMatrix . association

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
payoff a b = toRational $ ((total_payoff a b) + (total_payoff b a)) / 2
    where
        -- Total payoff for X succesfully communicating with Y
        total_payoff x y = sum [(freq $ objects !! (i-1)) * success_payoff i x y | i <- [1..num_objects]]

        -- Payoff for X succesfully communicating "i" to Y
        success_payoff i x y = (id_payoff $ objects !! (i-1)) * (fromRational $ comm_success i x y)


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
