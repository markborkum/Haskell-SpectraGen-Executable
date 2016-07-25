{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module NMR.Quantity
( -- * Quantity class
  Quantity
  -- * SI units
, Dimensionless(..) , Angle(..) , Frequency(..) , Time(..)
) where

import Data.Data (Data)
import Data.Default (Default)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Math.FFT.Base (FFTWReal)
import System.Random (Random)

-- | The `Quantity` class is used for types that can be interpreted as quantities.
--
class (Functor f) => Quantity f
instance Quantity Dimensionless
instance Quantity Frequency
instance Quantity Angle
instance Quantity Time

-- | A dimensionless quantity (SI units: 1).
--
newtype Dimensionless a = Dimensionless { getDimensionless :: a }
	deriving (Functor, Enum, Eq, Ord, Read, Show, Num, Fractional, Floating, Integral, Real, RealFloat, RealFrac, Data, Default, FFTWReal, Random, Storable, Typeable)

-- | An angle (SI units: Radians).
--
newtype Angle a = Angle { getAngle :: a }
	deriving (Functor, Enum, Eq, Ord, Read, Show, Num, Fractional, Floating, Integral, Real, RealFloat, RealFrac, Data, Default, Typeable)

-- | A frequency (SI units: Hertz).
--
newtype Frequency a = Frequency { getFrequency :: a }
	deriving (Functor, Enum, Eq, Ord, Read, Show, Num, Fractional, Floating, Integral, Real, RealFloat, RealFrac, Data, Default, Typeable)

-- | A time (SI units: Seconds).
--
newtype Time a = Time { getTime :: a }
	deriving (Functor, Enum, Eq, Ord, Read, Show, Num, Fractional, Floating, Integral, Real, RealFloat, RealFrac, Data, Default, Typeable)
