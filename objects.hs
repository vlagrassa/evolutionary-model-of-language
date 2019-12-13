module Objects where

import System.Random


-- The objects/events identified by signals
data Object = Object {

    -- The frequency with which this object/event occurs
    -- How often discussing this object is relevant
    freq :: Double,

    -- The payoff for correct identification of / communication about this object/event
    id_payoff :: Double

} deriving (Show, Eq)


-- Generate an object with a random frequency and random payoff, both between 0 and 1
instance Random Object where
    randomR (a,b) g =
        let
            (d1, g')  = randomR (min (freq a) (freq b), max (freq a) (freq b)) g
            (d2, g'') = randomR (min (id_payoff a) (id_payoff b), max (id_payoff a) (id_payoff b)) g'
        in
            (Object d1 d2, g'')

    random = randomR (Object 0 0, Object 1 1)


total_payoff :: Object -> Double
total_payoff obj = freq obj * id_payoff obj
