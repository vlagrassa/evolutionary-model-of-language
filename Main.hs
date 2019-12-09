import System.Environment
import Data.Matrix


-- The number of signals
-- n = 5
num_signals = 5

-- The number of messages
-- m = 5
num_objects = 5

data Organism = Organism {

    -- Association matrix of signals and objects
    association :: Matrix Rational,

    -- Payoff for correct communication about some object
    comm_payoff :: [Rational]
}
    deriving (Show, Eq)

type Population = [Organism]


make_matrix = matrix num_signals num_objects

mapIndex :: (Int -> Int -> a -> b) -> Matrix a -> Matrix b
mapIndex f m = matrix (nrows m) (ncols m) $ \(i,j) -> f i j (m ! (i,j))


-- In the paper, this is denoted with P
-- Probability of using signal j for object i
send_matrix :: Organism -> Matrix Rational
send_matrix organism = mapIndex normalizeRow a
    where
        a = association organism
        normalizeRow i _ n = n / sum (getRow i a)

-- In the paper, this is denoted with Q
-- Probability of interpreting signal j as refering to object i
hear_matrix :: Organism -> Matrix Rational
hear_matrix organism = mapIndex normalizeCol a
    where
        a = association organism
        normalizeCol _ j n = n / sum (getCol j a)


-- In the paper, this is denoted with F
-- Payoff for communicating
payoff :: Organism -> Organism -> Rational
payoff a b = (sum success_matrix) / 2
    where
        -- Probability of either organism succesfully communicating "i" to the other with "j"
        success_matrix = make_matrix $ \(i,j) -> (atob i j) + (btoa j i)

        -- Probability of A succesfully communicating with B, and vice versa
        atob i j = (comm_payoff a !! (i-1)) * (comm a b i j)
        btoa j i = (comm_payoff b !! (j-1)) * (comm b a j i)

        -- Probability of X succesfully communicating "i" to Y with signal "j"
        comm x y i j = (send_matrix x ! (i,j)) * (hear_matrix y ! (j,i))


-- The average payoff for a population
avg_payoff :: Population -> Rational
avg_payoff pop = sum payoffs / weight
    where
        -- List of all payoffs
        payoffs = fmap (uncurry payoff) organisms

        -- List of all permuations of two organisms
        organisms = fmap (\(x,y) -> (pop !! x, pop !! y)) indices

        -- List of all permutations of two indices in the population
        indices = [(x,y) | x <- [0 .. length pop - 1], y <- [0 .. length pop - 1], x /= y]

        -- Total weight to divide by
        weight = realToFrac $ (length pop) * (length pop - 1)