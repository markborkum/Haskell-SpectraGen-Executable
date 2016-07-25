module NMR.Types where

import NMR.Quantity

-- | The `RealPart` type is used to tag other types as "real".
--
data RealPart = RealPart
	deriving (Eq, Ord, Read, Show)

-- | The `ImagPart` type is used to tag other types as "imaginary".
--
data ImagPart = ImagPart
	deriving (Eq, Ord, Read, Show)

-- | The `Ideal` type encapsulates a set of ideal sampling parameters.
--
data Ideal a = Ideal
	{ _idealNumDataPointsPowerOfTwo :: Int
	, _idealNumZeroFills :: Int
	, _idealDwellTime :: Time a
	} deriving (Eq, Read, Show)

-- | The `NonIdeal` type encapsulates a set of non-ideal sampling parameters.
--
data NonIdeal a = NonIdeal
	{ _nonIdealTransmitterPhase :: Angle a
	, _nonIdealQuadPhaseShiftError :: Angle a
	, _nonIdealAmplifierGainImbalance :: Dimensionless a
	, _nonIdealRealChannelDC :: Dimensionless a
	, _nonIdealImagChannelDC :: Dimensionless a
	} deriving (Eq, Read, Show)

-- | The `Nucleus` type encapsulates the ideal sampling parameters for a resonant nucleus.
--
data Nucleus a = Nucleus
	{ _nucleusInitialAmplitude :: Dimensionless a
	, _nucleusFrequency :: Frequency a
	, _nucleusDecayTimeUnit :: Time a
	, _nucleusPhaseAngle :: Angle a
	} deriving (Eq, Read, Show)
