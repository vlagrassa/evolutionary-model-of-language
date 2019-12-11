module Signals where

data Vowel = Vowel {
    -- The height of the tongue: 1 is high, 0 is low
    height :: Double,

    -- How far forward the tongue is in the mouth: 1 is front, 0 is back
    frontness :: Double,

    -- Whether or not the vowel is tense
    tense :: Bool,

    -- Whether or not the vowel is rounded
    rounded :: Bool
} deriving (Eq)

instance Show Vowel where
    show v = "[" ++ height_str ++ ", " ++ front_str ++ ", " ++ tense_str ++ ", " ++ round_str ++ "]"
        where
            height_str = "height: " ++ (show . height $ v)
            front_str = "front: " ++ (show . frontness $ v)
            tense_str = if tense v then "+tense" else "-tense"
            round_str = if rounded v then "+round" else "-round"



data PlaceOfArticulation = Labial | Coronal | Velar | Glottal deriving (Eq, Show)
data MannerOfArticulation = Plosive | Nasal | Fricative | Approximant deriving (Eq, Show)
data VoiceOnset = Voiced | Unvoiced | Aspirated | Ejective deriving (Eq, Show)

data Consonant = Consonant {
    voice :: VoiceOnset,
    place :: PlaceOfArticulation,
    manner :: MannerOfArticulation
} deriving (Eq)

instance Show Consonant where
    show c = case c of
        Consonant p m v -> "[" ++ (show v) ++ " " ++ (show p) ++ " " ++ (show m) ++ "]"



class Dist a where
    -- Difference operator: returns 0 if identical, 1 if as far as possible
    (>~<) :: a -> a -> Double
    x >~< y = 1 - (x <~> y)

    -- Similarity operator: returns 1 if identical, 0 if as far as possible
    (<~>) :: a -> a -> Double
    x <~> y = 1 - (x >~< y)


instance Dist Bool where
    b1 >~< b2 = if b1 == b2 then 0 else 1


instance Dist Vowel where
    v1 >~< v2 = sum . zipWith (*) [space_diff, round_diff, tense_diff] $ [0.6, 0.3, 0.1]
        where
            h = abs $ height v1 - height v2
            mod_height v = (height v / 2) + 0.5

            total_width = abs $ frontness v1 - frontness v2
            wid1 = mod_height v1 * total_width
            wid2 = mod_height v2 * total_width
            space_diff = (((sqrt $ h^2 + wid1^2) + (sqrt $ h^2 + wid2^2)) / 2) / max_space_diff

            -- max_space_diff = ((sqrt $ 1 + 1) + (sqrt $ 1 + 0.5^2)) / 2
            max_space_diff = (sqrt 2 + sqrt 1.25) / 2

            round_diff = rounded v1 >~< rounded v2
            tense_diff = tense v1 >~< tense v2


instance Dist PlaceOfArticulation where
    c1 >~< c2 = if c1 == c2 then 0 else 1

instance Dist MannerOfArticulation where
    c1 >~< c2 = if c1 == c2 then 0 else 1

instance Dist VoiceOnset where
    Voiced    >~< Unvoiced  = 0.3
    Voiced    >~< Aspirated = 0.6
    Voiced    >~< Ejective  = 1.0
    Unvoiced  >~< Aspirated = 0.25
    Unvoiced  >~< Ejective  = 0.9
    Aspirated >~< Ejective  = 0.7
    c1        >~< c2        = if c1 == c2 then 0 else c2 >~< c1

instance Dist Consonant where
    c1 >~< c2 = sum . zipWith (*) [voice_diff, place_diff, manner_diff] $ [0.3, 0.35, 0.35]
        where
            voice_diff = voice c1 >~< voice c2
            place_diff = place c1 >~< place c2
            manner_diff = manner c1 >~< manner c2