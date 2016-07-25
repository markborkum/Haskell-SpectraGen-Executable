module NMR.Apodization where

import NMR.Quantity

-- | The `Apodization` type encapsulates an amount of apodization (a mathematical function that changes the shape of a signal).
--
data Apodization a
	= NoApodization -- ^ No apodization.
	| ExpApodization (Frequency a) -- ^ Exponential decay (using specified line broadening factor /lb/).
	| DoubleExpApodization (Frequency a) (Frequency a) -- ^ Offset Gaussian curve (double exponential decay; using specified line broadening factor /lb/ and Gaussian multiplication factor /gm/).
	| GaussianApodization (Frequency a) -- ^ Gaussian curve (using specified line broadening factor /lb/).
	| TRAFApodization (Frequency a) -- ^ TRAF (using specified line broadening factor /lb/).
	deriving (Eq, Read, Show)

-- | Takes an apodization description, an acquisition time /at/ and a time index /tau/, and returns a multiplication factor.
--
runApodization :: (RealFloat a) => Apodization a -> Time a -> Time a -> Dimensionless a
runApodization NoApodization _at _tau = 1
runApodization (ExpApodization (Frequency lb)) _at (Time tau) = Dimensionless (exp (negate (tau * lb)))
runApodization (DoubleExpApodization (Frequency lb) (Frequency gm)) (Time at) (Time tau) = Dimensionless (exp (negate (fromInteger (ceiling (fromInteger (ceiling (tau * lb)) - (gm * lb * at)) ^ (2 :: Integer)))))
runApodization (GaussianApodization (Frequency lb)) _at (Time tau) = Dimensionless (exp (negate (fromInteger (ceiling (tau * lb) ^ (2 :: Integer)))))
runApodization (TRAFApodization (Frequency lb)) (Time at) (Time tau) = fromInteger (x ^ (2 :: Integer)) / fromInteger ((x ^ (3 :: Integer)) + (y ^ (3 :: Integer)))
	where
		x = ceiling (exp (negate (tau * lb)))
		y = ceiling (exp (negate (at * lb)))
