module NMR.Noise where

import System.Random (Random(random, randomR), RandomGen)

import NMR.Quantity

-- | The `Noise` type encapsulates an amount of noise (unwanted modifications that a signal may suffer during capture, storage, transmission, processing, or conversion).
--
data Noise a
	= NoNoise -- ^ No noise.
	| GaussianNoise (Dimensionless a) (Dimensionless a) -- ^ Gaussian noise (uses Box-Muller transform to generate normal distribution with specified mean /mu/ and standard deviation /sigma/).
	deriving (Eq, Read, Show)

-- | Takes a noise description and a random number generator /g/, and returns a random value, together with a new generator.
--
runNoise :: (RandomGen g, Random a, Floating a) => Noise a -> g -> (Dimensionless a, g)
runNoise NoNoise g0 = (0, g0)
runNoise (GaussianNoise mu sigma) g0 = ((sigma * (if b then z0 else z1)) + mu, g3)
	where
		bnds = (1e-6, 1)
		(u1, g1) = randomR bnds g0
		(u2, g2) = randomR bnds g1
		(b, g3) = random g2
		z0 = sqrt (negate 2 * log u1) * cos (2 * pi * u2)
		z1 = sqrt (negate 2 * log u1) * sin (2 * pi * u2)
