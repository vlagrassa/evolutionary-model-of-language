module Signals where

data Vowel = Vowel {
    -- The height of the tongue: 1 is high, 0 is low
    height :: Double,

    -- How far forward the tongue is in the mouth: 1 is front, 0 is back
    frontness :: Double,

    -- Whether or not the vowel is tense
    tense :: Bool,

    -- Whether or not the vowel is rounded
    rounded :: Bool,

    char_v :: Char
} deriving (Eq)

instance Show Vowel where
    show v = rep : " [" ++ height_str ++ ", " ++ front_str ++ ", " ++ tense_str ++ ", " ++ round_str ++ "]"
        where
            height_str = "height: " ++ (show . height $ v)
            front_str = "front: " ++ (show . frontness $ v)
            tense_str = if tense v then "+tense" else "-tense"
            round_str = if rounded v then "+round" else "-round"
            rep = char_v v



data PlaceOfArticulation = Labial | Coronal | Velar | Glottal deriving (Eq, Show)
data MannerOfArticulation = Plosive | Nasal | Fricative | Approximant deriving (Eq, Show)
data VoiceOnset = Voiced | Unvoiced | Aspirated | Ejective deriving (Eq, Show)

data Consonant = Consonant {
    voice :: VoiceOnset,
    place :: PlaceOfArticulation,
    manner :: MannerOfArticulation,
    char_c :: Char
} deriving (Eq)

instance Show Consonant where
    show c = case c of
        Consonant p m v c -> c : " [" ++ (show v) ++ " " ++ (show p) ++ " " ++ (show m) ++ "]"



-- Data types with some notion of similarity / maximum distance between objects
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


data Syllable = Syllable {
    initial :: Consonant,
    medial :: Vowel,
    final :: Consonant
} deriving (Eq)

instance Show Syllable where
    show s = case s of Syllable i m f -> char_c i : char_v m : char_c f : []

instance Dist Syllable where
    s1 >~< s2 = sum $ zipWith (*) [0.25, 0.5, 0.25] [ini_diff, med_diff, fin_diff]
        where
            ini_diff = initial s1 >~< initial s2
            med_diff = medial s1 >~< medial s2
            fin_diff = final s1 >~< final s2


v1 = Vowel 1 1 True False 'i'
v2 = Vowel 1 0 True True 'u'
v3 = Vowel 0 0 True True 'o'
v4 = Vowel 0 1 False False 'a'
v5 = Vowel 0.5 0.85 False False 'e'

c_b = Consonant Voiced Labial Plosive 'b'
c_p = Consonant Unvoiced Labial Plosive 'p'
c_k' = Consonant Ejective Velar Plosive  'k'
c_s = Consonant Unvoiced Coronal Fricative 's'
