module Text.IPA where

data Phoneme
  = PulmonicConsonant Place Manner Voicing
  | Click Place
  | VoicedImplosive Place
  | Ejective Phoneme
  | VoicessLabialVelarFricative
  | VoicedLabialVelarApproximant
  | VoicedLabialPalatalApproximant
  | EpiglottalFricative Voicing
  | EpiglottalPlosive
  | AlveoloPalatalFricative Voicing
  | VoicedAlveolarLateralFlap
  | SJSound -- "simultaneous [ʃ] and [x]"
  | Affricate Phoneme Phoneme
  | DoubleArticulation Phoneme Phoneme
  | Vowel Backness Height Roundedness
  | Stressed Stressedness Phoneme
  | ExplicitLength Length Phoneme
  | WithTone Tone Phoneme
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
  | TapOrFlap
  | Fricative
  | LateralFricative
  | Approximant
  | LateralApproximant
  deriving (Eq, Show)

data Voicing = Voiceless | Voiced deriving (Bounded, Enum, Eq, Ord, Show)

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
