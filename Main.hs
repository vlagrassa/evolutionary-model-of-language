import System.Environment
import Data.Matrix

data Organism = Organism {

    -- Association matrix of signals and objects
    association :: Matrix Rational,

    -- Payoff for correct communication about some object
    comm_payoff :: [Rational]
}
    deriving (Show, Eq)


mapIndex :: (Int -> Int -> a -> b) -> Matrix a -> Matrix b
mapIndex f m = matrix (nrows m) (ncols m) $ \(i,j) -> f i j (m ! (i,j))


-- In the paper, this is denoted with P
send_matrix :: Organism -> Matrix Rational
send_matrix organism = mapIndex normalizeRow a
    where
        a = association organism
        normalizeRow i _ n = n / sum (getRow i a)

-- In the paper, this is denoted with Q
hear_matrix :: Organism -> Matrix Rational
hear_matrix organism = mapIndex normalizeCol a
    where
        a = association organism
        normalizeCol _ j n = n / sum (getCol j a)


-- In the paper, this is denoted with F
payoff :: Organism -> Organism -> Rational
payoff a b = (sum success_matrix) / 2
    where
        -- Probability of either organism succesfully communicating "i" to the other with "j"
        success_matrix = matrix 3 3 $ \(i,j) -> (atob i j) + (btoa j i)

        -- Probability of A succesfully communicating with B, and vice versa
        atob i j = (comm_payoff a !! i) * (comm a b i j)
        btoa j i = (comm_payoff b !! j) * (comm b a j i)

        -- Probability of X succesfully communicating "i" to Y with signal "j"
        comm x y i j = (send_matrix x ! (i,j)) * (hear_matrix y ! (j,i))
