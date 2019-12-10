module Organism where

import Data.Matrix
import System.Random
import Vars


type Association = Integer
type AssociationMatrix = Matrix Association

assocToRational :: Association -> Rational
-- assocToRational = id
assocToRational = realToFrac
assocToFractional  = fromRational . assocToRational


make_matrix = matrix num_signals num_objects



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
            assoc = make_assoc $ reverse . make_bin_list n $ (num_signals * num_objects)
            make_assoc m = (fromList num_signals num_objects m) :: AssociationMatrix

            -- Decompose input into list of 0s and 1s based on binary representation
            make_bin_list _ (-1) = []
            make_bin_list n index = if (n < 2 ^ index)
                then 0 : make_bin_list n (index-1)
                else 1 : make_bin_list (n - 2^index) (index-1)

    fromEnum org = fromInteger . fst $ foldl parse_bin_list (0,0) (association org)
        where
            parse_bin_list (acc, ind) curr = (acc + curr * (2 ^ ind), ind+1)


instance Bounded Organism where
    minBound = Organism (make_matrix $ \(i,j) -> 0) [1,1,1,1,1]
    maxBound = Organism (make_matrix $ \(i,j) -> 1) [1,1,1,1,1]


instance Random Organism where
    randomR (a, b) g = 
        case randomR (fromEnum a, fromEnum b) g of
            (o, g') -> (toEnum o, g')
    random g = randomR (minBound, maxBound) g