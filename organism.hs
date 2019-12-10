module Organism where

import Data.Matrix
import System.Random
import Vars


type Association = Integer

assocToRational :: Association -> Rational
-- assocToRational = id
assocToRational = realToFrac
assocToFractional  = fromRational . assocToRational



newtype AssociationMatrix = AssociationMatrix { getMatrix :: Matrix Association }
    deriving (Show, Eq)

matrix_a :: ((Int, Int) -> Association) -> AssociationMatrix
matrix_a = AssociationMatrix . matrix num_signals num_objects

fromList_a :: [Association] -> AssociationMatrix
fromList_a = AssociationMatrix . fromList num_signals num_objects



data Organism = Organism {

    -- Association matrix of signals and objects
    association :: AssociationMatrix,

    -- Payoff for correct communication about some object
    comm_payoff :: [Rational]
}
    deriving (Show, Eq)


instance Enum Organism where
    toEnum n = Organism assoc [1,1,1,1,1]
        where
            assoc = fromList_a . reverse . make_bin_list n $ num_signals * num_objects

            -- Decompose input into list of 0s and 1s based on binary representation
            make_bin_list _ (-1) = []
            make_bin_list n index = if (n < 2 ^ index)
                then 0 : make_bin_list n (index-1)
                else 1 : make_bin_list (n - 2^index) (index-1)

    fromEnum = fromInteger . fst . foldl parse_bin_list (0,0) . getOrgMatrix
        where
            parse_bin_list (acc, ind) curr = (acc + curr * (2 ^ ind), ind+1)


instance Bounded Organism where
    minBound = Organism (matrix_a $ \(i,j) -> 0) [1,1,1,1,1]
    maxBound = Organism (matrix_a $ \(i,j) -> 1) [1,1,1,1,1]


instance Random Organism where
    randomR (a, b) g = 
        case randomR (fromEnum a, fromEnum b) g of
            (o, g') -> (toEnum o, g')
    random g = randomR (minBound, maxBound) g


getOrgMatrix :: Organism -> Matrix Association
getOrgMatrix = getMatrix . association
