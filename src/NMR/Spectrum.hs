{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module NMR.Spectrum
( -- * Channel type
  Channel
  -- ** Constructors
, realChannel , imagChannel
  -- ** Modifiers
, withApodization , withNoise
  -- * Spectrum type
, Spectrum
  -- ** Constructors
, fromChannel
  -- ** Key-value pairs
, fromTimeSpectrum , fromFrequencySpectrum
  -- ** DFT
, dft , idft
  -- ** Quadrature detection
, realQuadL , realQuadR , imagQuadL , imagQuadR
  -- ** Lifts
, liftSpectrum , liftSpectrum2 , liftSpectrum3
) where

import Control.Arrow (first)
import Control.Monad (liftM, liftM2, liftM3)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Reader (ReaderT(..), withReaderT)
import Data.Array.CArray (CArray, liftArray, liftArray2, liftArray3)
import Data.Array.IArray (IArray(bounds), accumArray, amap, assocs, elems, ixmap)
import Data.Complex (Complex(..), imagPart, realPart)
import Data.Foldable (Foldable(foldMap))
import Data.Ix (range, rangeSize)
import Data.Monoid (Sum(..))
import Data.Normalize (Normalize(normalize))
import Data.Numeric.Function ()
import Data.Tagged (Tagged(..))
import Foreign.Storable (Storable)
import qualified Math.FFT
import Math.FFT.Base (FFTWReal)
import System.Random (Random, RandomGen)

import NMR.Apodization
import NMR.Noise
import NMR.Quantity
import NMR.Types
import NMR.Util

-- | The `Channel` type encapsulates a channel whose time-dependent signal can be sampled.
--
data Channel :: (* -> *) -> * -> * where
	MkChannel :: ReaderT (Time a) m (Dimensionless a) -> Channel m a

-- | Takes a set of non-ideal sampling parameters and a list of resonant nuclei, and returns a "real" time-dependent signal.
--
realChannel :: (Monad m, Foldable f, Floating a) => NonIdeal a -> f (Nucleus a) -> Tagged RealPart (Channel m a)
realChannel nonIdeal nuclei = Tagged (MkChannel (Reader.reader ((+ dc) . getSum (foldMap (Sum . f) nuclei))))
	where
		dc = _nonIdealRealChannelDC nonIdeal * getSum (foldMap (Sum . _nucleusInitialAmplitude) nuclei)
		psi = getAngle (_nonIdealTransmitterPhase nonIdeal)
		f nucleus (Time tau) = (a * Dimensionless (exp (negate lambda * tau) * cos ((omega * tau) + theta + psi)))
			where
				a = _nucleusInitialAmplitude nucleus
				lambda = recip (getTime (_nucleusDecayTimeUnit nucleus))
				omega = 2 * pi * getFrequency (_nucleusFrequency nucleus)
				theta = getAngle (_nucleusPhaseAngle nucleus)

-- | Takes a set of non-ideal sampling parameters and a list of resonant nuclei, and returns an "imaginary" time-dependent signal.
--
imagChannel :: (Monad m, Foldable f, Floating a) => NonIdeal a -> f (Nucleus a) -> Tagged ImagPart (Channel m a)
imagChannel nonIdeal nuclei = Tagged (MkChannel (Reader.reader ((+ dc) . getSum (foldMap (Sum . f) nuclei))))
	where
		dc = _nonIdealImagChannelDC nonIdeal * getSum (foldMap (Sum . _nucleusInitialAmplitude) nuclei)
		g = 1 + _nonIdealAmplifierGainImbalance nonIdeal
		phi = getAngle (_nonIdealQuadPhaseShiftError nonIdeal)
		psi = getAngle (_nonIdealTransmitterPhase nonIdeal)
		f nucleus (Time tau) = (g * a * Dimensionless (exp (negate lambda * tau) * sin ((omega * tau) + theta + phi + psi)))
			where
				a = _nucleusInitialAmplitude nucleus
				lambda = recip (getTime (_nucleusDecayTimeUnit nucleus))
				omega = 2 * pi * getFrequency (_nucleusFrequency nucleus)
				theta = getAngle (_nucleusPhaseAngle nucleus)

-- | Takes an apodization description and a channel, and returns a new "apodized" channel.
--
withApodization :: (MonadReader (Ideal a) m, RealFloat a) => Apodization a -> Channel m a -> Channel m a
withApodization r (MkChannel mf) = MkChannel (Trans.lift Reader.ask >>= \ideal -> liftM2 (*) (Reader.reader (runApodization r (getAcquisitionTime (_idealDwellTime ideal) (2 ^ _idealNumDataPointsPowerOfTwo ideal)))) mf)

-- | Takes a noise description and a channel, and returns a new "noisy" channel.
--
withNoise :: (MonadState g m, RandomGen g, Random a, Floating a) => Noise a -> Channel m a -> Channel m a
withNoise r (MkChannel mf) = MkChannel (liftM2 (+) (State.state (runNoise r)) mf)

-- | The `Spectrum` type encapsulates a sampled signal (represented as an immutable `CArray`).
--
data Spectrum :: (* -> *) -> (* -> *) -> * -> * where
	MkSpectrum :: (Quantity dom) => IdentityT m (CArray Int (Complex (Dimensionless a))) -> Spectrum dom m a

-- | Divide by the maximum real-part of the spectrum.
--
instance (Monad m, RealFloat a, Storable a) => Normalize (Spectrum dom m a) where
	normalize (MkSpectrum ma) = MkSpectrum (liftM (\a -> amap (/ (maximum (map realPart (elems a)) :+ 0)) a) ma)

-- | Takes a channel and returns a time-dependent spectrum.
--
fromChannel :: (MonadReader (Ideal a) m, RealFloat a, Storable a) => Channel m a -> Spectrum Time m a
fromChannel (MkChannel mf) = MkSpectrum (IdentityT (Reader.ask >>= \ideal -> let bnds@(lo, hi) = (0, (2 ^ _idealNumDataPointsPowerOfTwo ideal) - 1) in liftM (accumArray (+) 0 (lo, hi + (rangeSize bnds * ((2 ^ (_idealNumZeroFills ideal)) - 1)))) (mapM (\i -> runReaderT (liftM (\x -> (i, x :+ 0)) (withReaderT (getAcquisitionTime (_idealDwellTime ideal)) mf)) i) (range bnds))))

-- | Takes a time-dependent spectrum and returns a list of key-value pairs.
--
fromTimeSpectrum :: (MonadReader (Ideal a) m, Num a, Storable a) => Spectrum Time m a -> m [(Time a, Complex (Dimensionless a))]
fromTimeSpectrum (MkSpectrum ma) = runIdentityT (Reader.ask >>= \ideal -> liftM (f ideal) ma)
	where
		f ideal a = map (first (getAcquisitionTime (_idealDwellTime ideal))) (assocs a)

-- | Takes a frequency-dependent spectrum and returns a list of key-value pairs.
--
fromFrequencySpectrum :: (MonadReader (Ideal a) m, Fractional a, Storable a) => Spectrum Frequency m a -> m [(Frequency a, Complex (Dimensionless a))]
fromFrequencySpectrum (MkSpectrum ma) = runIdentityT (Reader.ask >>= \ideal -> liftM (f (_idealDwellTime ideal)) ma)
	where
		f dw a = map (first (Frequency . (/ at) . fromInteger . toInteger . subtract half)) (assocs a)
			where
				n = rangeSize (bounds a)
				half = n `div` 2
				(Time at) = getAcquisitionTime dw n

-- | 1-dimensional complex DFT.
--
dft :: (Monad m, FFTWReal a, Storable a) => Spectrum Time m a -> Spectrum Frequency m a
dft (MkSpectrum ma) = MkSpectrum (liftM (swap_ . Math.FFT.dft) ma)

-- | 1-dimensional complex inverse DFT.  Inverse of `dft`.
--
idft :: (Monad m, FFTWReal a, Storable a) => Spectrum Frequency m a -> Spectrum Time m a
idft (MkSpectrum ma) = MkSpectrum (liftM (Math.FFT.idft . swap_) ma)

swap_ :: (IArray a e) => a Int e -> a Int e
swap_ a = ixmap bnds (\i -> (if i >= half then (-) else (+)) i half) a
	where
		bnds = bounds a
		n = rangeSize bnds
		half = n `div` 2

-- | Real spectrum (negative frequencies).
--
realQuadL :: (Monad m, RealFloat a, Storable a) => Tagged RealPart (Spectrum Frequency m a) -> Tagged ImagPart (Spectrum Frequency m a) -> Tagged RealPart (Spectrum Frequency m a)
realQuadL (Tagged xL) (Tagged xR) = Tagged (liftSpectrum2 (\cL cR -> (realPart cL + imagPart cR) :+ 0) xL xR)

-- | Real spectrum (positive frequencies).
--
realQuadR :: (Monad m, RealFloat a, Storable a) => Tagged RealPart (Spectrum Frequency m a) -> Tagged ImagPart (Spectrum Frequency m a) -> Tagged RealPart (Spectrum Frequency m a)
realQuadR (Tagged xL) (Tagged xR) = Tagged (liftSpectrum2 (\cL cR -> (realPart cL - imagPart cR) :+ 0) xL xR)

-- | Imaginary spectrum (negative frequencies).
--
imagQuadL :: (Monad m, RealFloat a, Storable a) => Tagged ImagPart (Spectrum Frequency m a) -> Tagged RealPart (Spectrum Frequency m a) -> Tagged ImagPart (Spectrum Frequency m a)
imagQuadL (Tagged xL) (Tagged xR) = Tagged (liftSpectrum2 (\cL cR -> (realPart cL - imagPart cR) :+ 0) xL xR)

-- | Imaginary spectrum (positive frequencies).
--
imagQuadR :: (Monad m, RealFloat a, Storable a) => Tagged ImagPart (Spectrum Frequency m a) -> Tagged RealPart (Spectrum Frequency m a) -> Tagged ImagPart (Spectrum Frequency m a)
imagQuadR (Tagged xL) (Tagged xR) = Tagged (liftSpectrum2 (\cL cR -> (realPart cL + imagPart cR) :+ 0) xL xR)

-- | 1-spectrum lift.
--
liftSpectrum :: (Monad m, Quantity dom, Quantity dom', Storable a, Storable b) => (Complex (Dimensionless a) -> Complex (Dimensionless b)) -> Spectrum dom m a -> Spectrum dom' m b
liftSpectrum f (MkSpectrum ma0) = MkSpectrum (liftM (liftArray f) ma0)

-- | 2-spectrum lift.
--
liftSpectrum2 :: (Monad m, Quantity dom, Quantity dom', Quantity dom'', Storable a, Storable b, Storable c) => (Complex (Dimensionless a) -> Complex (Dimensionless b) -> Complex (Dimensionless c)) -> Spectrum dom m a -> Spectrum dom' m b -> Spectrum dom'' m c
liftSpectrum2 f (MkSpectrum ma0) (MkSpectrum ma1) = MkSpectrum (liftM2 (liftArray2 f) ma0 ma1)

-- | 3-spectrum lift.
--
liftSpectrum3 :: (Monad m, Quantity dom, Quantity dom', Quantity dom'', Quantity dom''', Storable a, Storable b, Storable c, Storable d) => (Complex (Dimensionless a) -> Complex (Dimensionless b) -> Complex (Dimensionless c) -> Complex (Dimensionless d)) -> Spectrum dom m a -> Spectrum dom' m b -> Spectrum dom'' m c -> Spectrum dom''' m d
liftSpectrum3 f (MkSpectrum ma0) (MkSpectrum ma1) (MkSpectrum ma2) = MkSpectrum (liftM3 (liftArray3 f) ma0 ma1 ma2)
