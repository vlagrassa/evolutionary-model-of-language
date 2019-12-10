import System.Environment
import Data.Matrix


type Association = Integer
type AssociationMatrix = Matrix Association

assocToRational :: Association -> Rational
-- assocToRational = id
assocToRational = realToFrac


-- The number of signals
-- In the paper, this is denoted with n
num_signals = 5

-- The number of messages
-- In the paper, this is denoted with m
num_objects = 5

simil_matrix = matrix num_signals num_signals $
    \(i,j) -> (realToFrac 1) / (realToFrac . (+) 1 . abs $ i - j)

error_matrix = matrix num_signals num_signals $
    \(i,j) -> (simil_matrix ! (i,j)) / (sum . getRow i $ simil_matrix)


data Organism = Organism {

    -- Association matrix of signals and objects
    association :: AssociationMatrix,

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
        normalizeRow :: Int -> Int -> Association -> Rational
        normalizeRow i _ n = (assocToRational n) / (assocToRational . sum . getRow i $ a)

-- In the paper, this is denoted with Q
-- Probability of interpreting signal j as refering to object i
hear_matrix :: Organism -> Matrix Rational
hear_matrix organism = mapIndex normalizeCol a
    where
        a = association organism
        normalizeCol :: Int -> Int -> Association -> Rational
        normalizeCol _ j n = (assocToRational n) / (assocToRational . sum . getCol j $ a)


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
        prob_error j k = error_matrix ! (j,k)

        -- Probability that listener will infer object "i" from signal "k"
        prob_hear k = hear_matrix y ! (i,k)



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