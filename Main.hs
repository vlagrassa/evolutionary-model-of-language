import System.Environment
import Data.Matrix
import System.Random
import Data.List
import Control.Monad.Trans.State
import Control.Monad

import Organism
import Vars
import Utils
import Objects
import Fitness
import Children



data Generation = Generation {
    gen_index  :: Int,
    population :: Population,
    generator  :: StdGen
} deriving (Show)

data GenSummary = GenSummary {
    gen_index_ :: Int,
    population_size :: Int,
    average_fitness :: Rational,
    highest_fitness :: Rational,
    average_assoc_m :: Matrix Rational,
    highest_assoc_m :: AssociationMatrix
} | BlankGeneration {gen_index__ :: Int}

instance Show GenSummary where
    show (GenSummary idx size fit_avg fit_hi assoc_avg assoc_hi) =
        "\nGeneration " ++ (show idx) ++ ":\n" ++
        "  Population Size: " ++ (show size) ++ "\n" ++
        "  Average Fitness: " ++ (show . realToFrac $ fit_avg) ++ " (" ++ (show . roundN 1 . (*) 100 $ fit_avg) ++"%)\n" ++
        "  Highest Fitness: " ++ (show . realToFrac $ fit_hi) ++ "\n" ++
        "  Average Association:\n" ++ (show . fmap (roundN 5) $ assoc_avg) ++ "\n" ++
        "  Highest Association:\n" ++ (show . getMatrix $ assoc_hi)  ++ "\n"
        -- fmap (roundN 5 . assocToRational)
    show (BlankGeneration idx) =
        "\nGeneration " ++ (show idx) ++ ":\n" ++
        "  Population Size: " ++ (show 0) ++ "\n:(\n"


summarize_gen :: Int -> Population -> GenSummary
summarize_gen idx pop = GenSummary idx (length pop) (avg_fitness_n pop) (snd most_fit) (avg_association pop) (association . fst $ most_fit)
    where
        most_fit = fittest pop



-- Function to create a new population of organisms from the current population
create_next_generation :: Population -> State StdGen Population
create_next_generation pop = do
    gen <- get
    let (children, new_gen) = foldl fold_func ([], gen) parents
    put new_gen
    return $ take carrying_capacity children
    where
        parents = sortBy (\(_,a) (_,b) -> compare b a) $ zip pop (scaled_fitness_arr pop)

        assoc = assoc_fitness pop

        fold_func (lis, g) (org, fit) = (lis ++ new_lis, g') where
            (new_lis, g') = runState (gen_children_fit assoc fit org) g


-- Function to move to the next generation
next_generation :: State Generation GenSummary
next_generation = do
    Generation idx pop gen <- get
    if length pop == 0
        then do
            put $ Generation (idx) pop gen
            return $ BlankGeneration (idx+1) -- summarize_gen (idx+1) pop
        else do
            let fit = fitness_arr pop
            let (children, new_gen) = runState (create_next_generation pop) gen
            put $ Generation (idx+1) children new_gen
            return $ summarize_gen (idx) pop


run_n_generations :: Int -> State Generation [GenSummary]
run_n_generations n = replicateM n next_generation
