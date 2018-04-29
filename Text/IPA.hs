module Text.IPA where

import Data.Maybe (fromJust)

data Phoneme
  = PulmonicConsonant Voicing Place Manner
  | Click ClickType
  | VoicedImplosive Place
  | Ejective Phoneme
  | VoicelessLabialVelarFricative
  | VoicedLabialVelarApproximant
  | VoicedLabialPalatalApproximant
  | EpiglottalFricative Voicing
  | EpiglottalPlosive
  | AlveoloPalatalFricative Voicing
  | VoicedAlveolarLateralFlap
  | SJSound -- "simultaneous [ʃ] and [x]"
  | Affricate Phoneme Phoneme
  | DoubleArticulation Phoneme Phoneme
  | Vowel Height Backness Roundedness
  | Stressed Stressedness Phoneme
  | ExplicitLength Length Phoneme
  | WithTone Tone Phoneme
  | WithDiacritic Diacritic Phoneme
  deriving Eq

data Place
  = Bilabial
  | Labiodental
  | Dental
  | Alveolar
  | Postalveolar
  | Retroflex
  | Palatal
  | Velar
  | Uvular
  | Pharyngeal
  | Glottal
  deriving (Eq, Ord, Show)

data Manner
  = Plosive
  | Nasal
  | Trill
  | Tap
  | Fricative
  | LateralFricative
  | Approximant
  | LateralApproximant
  deriving (Eq, Show)

data Voicing = Voiceless | Voiced deriving (Bounded, Enum, Eq, Ord, Show)

data ClickType
 = BilabialClick
 | DentalClick
 | PostalveolarClick
 | PalatoalveolarClick
 | AlveolarLateralClick
 deriving (Eq, Show)

data Backness
  = Front
  | NearFront
  | Central
  | NearBack
  | Back
  deriving (Eq, Ord, Show)

data Height
  = Close
  | NearClose
  | CloseMid
  | MidHeight
  | OpenMid
  | NearOpen
  | Open
  deriving (Eq, Ord, Show)

data Roundedness = Unrounded | Rounded deriving (Bounded, Enum, Eq, Ord, Show)

data Stressedness = Primary | Secondary deriving (Eq, Show)

data Length = Long | HalfLong | Short | ExtraShort deriving (Eq, Ord, Show)

data Tone
  = ExtraHigh
  | High
  | MidTone
  | Low
  | ExtraLow
  | Rising
  | Falling
  | HighRising
  | LowRising
  | RisingFalling
  deriving (Eq, Show)

data Diacritic
  = ExplicitVoicing Voicing
  | Aspirated
  | MoreRounded
  | LessRounded
  | Advanced
  | Retracted
  | Centralized
  | MidCentralized
  | Syllabic
  | NonSyllabic
  | Rhotacized
  | Breathy
  | Creaky
  | Linguolabial
  | Labialized
  | Palatalized
  | Velarized
  | Pharyngealized
  | Raised
  | Lowered
  | AdvancedTongueRoot
  | RetractedTongueRoot
  | DentalDiacritic
  | Apical
  | Laminal
  | Nasalized
  | NasalRelease
  | LateralRelease
  | NoAudibleRelease
  deriving (Eq, Show)


toUnicode :: Phoneme -> Maybe String
toUnicode (PulmonicConsonant Voiceless Bilabial Plosive) = Just "p"
toUnicode (PulmonicConsonant Voiced Bilabial Plosive) = Just "b"
toUnicode (PulmonicConsonant Voiced Bilabial Nasal) = Just "m"
toUnicode (PulmonicConsonant Voiced Bilabial Trill) = Just "ʙ"
toUnicode (PulmonicConsonant Voiceless Bilabial Fricative) = Just "ɸ"
toUnicode (PulmonicConsonant Voiced Bilabial Fricative) = Just "β"

toUnicode (PulmonicConsonant Voiced Labiodental Nasal) = Just "ɱ"
toUnicode (PulmonicConsonant Voiced Labiodental Tap) = Just "ⱱ"
toUnicode (PulmonicConsonant Voiceless Labiodental Fricative) = Just "f"
toUnicode (PulmonicConsonant Voiced Labiodental Fricative) = Just "v"
toUnicode (PulmonicConsonant Voiced Labiodental Approximant) = Just "ʋ"

toUnicode (PulmonicConsonant Voiceless Retroflex Plosive) = Just "ʈ"
toUnicode (PulmonicConsonant Voiced Retroflex Plosive) = Just "ɖ"
toUnicode (PulmonicConsonant Voiced Retroflex Nasal) = Just "ɳ"
toUnicode (PulmonicConsonant Voiced Retroflex Tap) = Just "ɽ"
toUnicode (PulmonicConsonant Voiceless Retroflex Fricative) = Just "ʂ"
toUnicode (PulmonicConsonant Voiced Retroflex Fricative) = Just "ʐ"
toUnicode (PulmonicConsonant Voiced Retroflex Approximant) = Just "ɻ"
toUnicode (PulmonicConsonant Voiced Retroflex LateralApproximant) = Just "ɭ"

toUnicode (PulmonicConsonant Voiceless Palatal Plosive) = Just "c"
toUnicode (PulmonicConsonant Voiced Palatal Plosive) = Just "ɟ"
toUnicode (PulmonicConsonant Voiced Palatal Nasal) = Just "ɲ"
toUnicode (PulmonicConsonant Voiceless Palatal Fricative) = Just "ç"
toUnicode (PulmonicConsonant Voiced Palatal Fricative) = Just "ʝ"
toUnicode (PulmonicConsonant Voiced Palatal Approximant) = Just "j"
toUnicode (PulmonicConsonant Voiced Palatal LateralApproximant) = Just "ʎ"

toUnicode (PulmonicConsonant Voiceless Velar Plosive) = Just "k"
toUnicode (PulmonicConsonant Voiced Velar Plosive) = Just "g"
toUnicode (PulmonicConsonant Voiced Velar Nasal) = Just "ŋ"
toUnicode (PulmonicConsonant Voiceless Velar Fricative) = Just "x"
toUnicode (PulmonicConsonant Voiced Velar Fricative) = Just "ɣ"
toUnicode (PulmonicConsonant Voiced Velar Approximant) = Just "ɰ"
toUnicode (PulmonicConsonant Voiced Velar LateralApproximant) = Just "ʟ"

toUnicode (PulmonicConsonant Voiceless Uvular Plosive) = Just "q"
toUnicode (PulmonicConsonant Voiced Uvular Plosive) = Just "ɢ"
toUnicode (PulmonicConsonant Voiced Uvular Nasal) = Just "ɴ"
toUnicode (PulmonicConsonant Voiced Uvular Trill) = Just "ʀ"
toUnicode (PulmonicConsonant Voiceless Uvular Fricative) = Just "χ"
toUnicode (PulmonicConsonant Voiced Uvular Fricative) = Just "ʁ"

toUnicode (PulmonicConsonant Voiceless Pharyngeal Fricative) = Just "ħ"
toUnicode (PulmonicConsonant Voiced Pharyngeal Fricative) = Just "ʕ"

toUnicode (PulmonicConsonant Voiceless Glottal Plosive) = Just "ʔ"
toUnicode (PulmonicConsonant Voiceless Glottal Fricative) = Just "h"
toUnicode (PulmonicConsonant Voiced Glottal Fricative) = Just "ɦ"

-- Dental, alveolar, postalveolar
toUnicode (PulmonicConsonant Voiceless _ Plosive) = Just "t"
toUnicode (PulmonicConsonant Voiced _ Plosive) = Just "d"
toUnicode (PulmonicConsonant Voiced _ Nasal) = Just "n"
toUnicode (PulmonicConsonant Voiced _ Trill) = Just "r"
toUnicode (PulmonicConsonant Voiced _ Tap) = Just "ɾ"
toUnicode (PulmonicConsonant Voiceless Dental Fricative) = Just "θ"
toUnicode (PulmonicConsonant Voiced Dental Fricative) = Just "ð"
toUnicode (PulmonicConsonant Voiceless Alveolar Fricative) = Just "s"
toUnicode (PulmonicConsonant Voiced Alveolar Fricative) = Just "z"
toUnicode (PulmonicConsonant Voiceless Postalveolar Fricative) = Just "ʃ"
toUnicode (PulmonicConsonant Voiced Postalveolar Fricative) = Just "ʒ"
toUnicode (PulmonicConsonant Voiceless _ LateralFricative) = Just "ɬ"
toUnicode (PulmonicConsonant Voiced _ LateralFricative) = Just "ɮ"
toUnicode (PulmonicConsonant Voiced _ Approximant) = Just "ɹ"
toUnicode (PulmonicConsonant Voiced _ LateralApproximant) = Just "l"

toUnicode (PulmonicConsonant _ _ _) = Nothing

toUnicode (Click c) =
  Just $ case c of
    BilabialClick -> "ʘ"
    DentalClick -> "ǀ"
    PostalveolarClick -> "ǃ"
    PalatoalveolarClick -> "ǂ"
    AlveolarLateralClick -> "ǁ"

toUnicode (VoicedImplosive p) =
  case p of
    Bilabial -> Just "ɓ"
    Dental -> Just "ɗ"
    Alveolar -> Just "ɗ"
    Palatal -> Just "ʄ"
    Velar -> Just "ɠ"
    Uvular -> Just "ʛ"
    _ -> Nothing

toUnicode (Ejective p) = (++"ʼ") <$> toUnicode p

toUnicode VoicelessLabialVelarFricative = Just "ʍ"
toUnicode VoicedLabialVelarApproximant = Just "w"
toUnicode VoicedLabialPalatalApproximant = Just "ɥ"
toUnicode (EpiglottalFricative Voiceless) = Just "ʜ"
toUnicode (EpiglottalFricative Voiced) = Just "ʢ"
toUnicode EpiglottalPlosive = Just "ʡ"
toUnicode (AlveoloPalatalFricative Voiceless) = Just "ɕ"
toUnicode (AlveoloPalatalFricative Voiced) = Just "ʑ"
toUnicode VoicedAlveolarLateralFlap = Just "ɺ"
toUnicode SJSound = Just "ɧ"
toUnicode (Affricate a b) = do
  aUnicode <- toUnicode a
  bUnicode <- toUnicode b
  return (aUnicode ++ "͡" ++ bUnicode)
toUnicode (DoubleArticulation a b) = do
  aUnicode <- toUnicode a
  bUnicode <- toUnicode b
  return (aUnicode ++ "͜" ++ bUnicode)

--  Vowel Height Backness Roundedness
toUnicode (Vowel Close Front Unrounded) = Just "i"
toUnicode (Vowel Close Front Rounded) = Just "y"
toUnicode (Vowel Close Central Unrounded) = Just "ɨ"
toUnicode (Vowel Close Central Rounded) = Just "ʉ"
toUnicode (Vowel Close Back Unrounded) = Just "ɯ"
toUnicode (Vowel Close Back Rounded) = Just "u"
toUnicode (Vowel NearClose NearFront Unrounded) = Just "ɪ"
toUnicode (Vowel NearClose NearFront Rounded) = Just "ʏ"
toUnicode (Vowel NearClose NearBack Rounded) = Just "ʊ"
toUnicode (Vowel CloseMid Front Unrounded) = Just "e"
toUnicode (Vowel CloseMid Front Rounded) = Just "ø"
toUnicode (Vowel CloseMid Central Unrounded) = Just "ɘ"
toUnicode (Vowel CloseMid Central Rounded) = Just "ɵ"
toUnicode (Vowel CloseMid Back Unrounded) = Just "ɤ"
toUnicode (Vowel CloseMid Back Rounded) = Just "o"
toUnicode (Vowel MidHeight Central _) = Just "ə"
toUnicode (Vowel OpenMid Front Unrounded) = Just "ɛ"
toUnicode (Vowel OpenMid Front Rounded) = Just "œ"
toUnicode (Vowel OpenMid Central Unrounded) = Just "ɜ"
toUnicode (Vowel OpenMid Central Rounded) = Just "ɞ"
toUnicode (Vowel OpenMid Back Unrounded) = Just "ʌ"
toUnicode (Vowel OpenMid Back Rounded) = Just "ɔ"
toUnicode (Vowel NearOpen Front Unrounded) = Just "æ"
toUnicode (Vowel NearOpen Central _) = Just "ɐ"
toUnicode (Vowel Open Front Unrounded) = Just "a"
toUnicode (Vowel Open Front Rounded) = Just "ɶ"
toUnicode (Vowel Open Back Unrounded) = Just "ɑ"
toUnicode (Vowel Open Back Rounded) = Just "ɒ"
toUnicode (Vowel _ _ _) = Nothing

toUnicode (Stressed Primary p) = ("ˈ"++) <$> toUnicode p
toUnicode (Stressed Secondary p) = ("ˌ"++) <$> toUnicode p

toUnicode (ExplicitLength Long p) = (++"ː") <$> toUnicode p
toUnicode (ExplicitLength HalfLong p) = (++"ˑ") <$> toUnicode p
toUnicode (ExplicitLength Short p) = toUnicode p
toUnicode (ExplicitLength ExtraShort p) = (++"̆") <$> toUnicode p

toUnicode (WithTone ExtraHigh p) = (++"̋") <$> toUnicode p
toUnicode (WithTone High p) = (++"́") <$> toUnicode p
toUnicode (WithTone MidTone p) = (++"̄") <$> toUnicode p
toUnicode (WithTone Low p) = (++"̀") <$> toUnicode p
toUnicode (WithTone ExtraLow p) = (++"̏") <$> toUnicode p
toUnicode (WithTone Rising p) = (++"̌") <$> toUnicode p
toUnicode (WithTone Falling p) = (++"̂") <$> toUnicode p
toUnicode (WithTone HighRising p) = (++"᷄") <$> toUnicode p
toUnicode (WithTone LowRising p) = (++"᷅") <$> toUnicode p
toUnicode (WithTone RisingFalling p) = (++"᷈") <$> toUnicode p

toUnicode (WithDiacritic d p) = (++(showDiacritic d)) <$> toUnicode p
  where
    showDiacritic (ExplicitVoicing Voiceless) = "\x0325"
    showDiacritic (ExplicitVoicing Voiced) = "\x032C"
    showDiacritic Aspirated = "\x02B0"
    showDiacritic MoreRounded = "\x0339"
    showDiacritic LessRounded = "\x031C"
    showDiacritic Advanced = "\x031F"
    showDiacritic Retracted = "\x0320"
    showDiacritic Centralized = "\x0308"
    showDiacritic MidCentralized = "\x033D"
    showDiacritic Syllabic = "\x0329"
    showDiacritic NonSyllabic = "\x032F"
    showDiacritic Rhotacized = "\x02DE"
    showDiacritic Breathy = "\x0324"
    showDiacritic Creaky = "\x0330"
    showDiacritic Linguolabial = "\x033C"
    showDiacritic Labialized = "\x02B7"
    showDiacritic Palatalized = "\x02B2"
    showDiacritic Velarized = "\x02E0"
    showDiacritic Pharyngealized = "\x02E4"
    showDiacritic Raised = "\x031D"
    showDiacritic Lowered = "\x031E"
    showDiacritic AdvancedTongueRoot = "\x0318"
    showDiacritic RetractedTongueRoot = "\x0319"
    showDiacritic DentalDiacritic = "\x032A"
    showDiacritic Apical = "\x033A"
    showDiacritic Laminal = "\x033B"
    showDiacritic Nasalized = "\x0303"
    showDiacritic NasalRelease = "\x207F"
    showDiacritic LateralRelease = "\x2E1"
    showDiacritic NoAudibleRelease = "\x031A"

instance Show Phoneme where
  show p = '/' : fromJust (toUnicode p) ++ "/"
