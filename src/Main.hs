{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (forM_, replicateM_, unless, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.RWS.Strict (runRWST)
import Control.Monad.Trans.Writer.Strict (execWriter)
import Control.Monad.Writer (MonadWriter)
import qualified Control.Monad.Writer as Writer
import Data.Complex (realPart)
import Data.Data (Data)
import Data.Default (Default(def))
import Data.Normalize (normalize)
import Data.Tagged (Tagged(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Pipes (Producer, (>->), runEffect, yield)
import qualified Pipes.Prelude as P
import System.Console.CmdArgs.Implicit ((&=), cmdArgs, details, explicit, groupname, help, helpArg, name, program, summary)
import System.Exit (exitFailure, exitSuccess)
import System.IO (Handle, hPutChar, hPutStr, hPutStrLn, stderr, stdout)
import System.Random (StdGen, getStdGen, setStdGen)
import Text.Printf (printf)

import NMR.Apodization
import NMR.Noise
import NMR.Quantity
import NMR.Spectrum
import NMR.Types
import NMR.Util

data SpectraGen a = SpectraGen
	{ _spectraGenNumDataPointsPowerOfTwo :: Int
	, _spectraGenNumZeroFills :: Int
	, _spectraGenDwellTime :: Time a
	, _spectraGenNuclei :: [(Dimensionless a, Frequency a, Time a, Angle a)]
	, _spectraGenScans :: [(Int, Angle a, Dimensionless a, Dimensionless a, Dimensionless a, Dimensionless a, Frequency a)]
	} deriving (Eq, Read, Show, Data, Typeable)

instance (Data a, Default a, Typeable a) => Default (SpectraGen a) where
	def = SpectraGen
		{ _spectraGenNumDataPointsPowerOfTwo = def &= groupname "Signal Acquisition" &= explicit &= name "data-points"  &= help "Number of data points sampled (power of two)."
		, _spectraGenNumZeroFills            = def &= groupname "Zero Filling"       &= explicit &= name "zero-fills"   &= help "Number of zero fills."
		, _spectraGenDwellTime               = def &= groupname "Signal Acquisition" &= explicit &= name "dwell-time"   &= help "Dwell time."
		, _spectraGenNuclei                  = def &= groupname "Nuclei"             &= explicit &= name "nucleus"      &= help "Nucleus (see details)."
		, _spectraGenScans                   = def &= groupname "Signal Acquisition" &= explicit &= name "scan"         &= help "Scan (see details)."
		} &= helpArg [groupname "Other"]
		  &= program "spectra-gen"
		  &= summary "spectra-gen v0.1.0.0, (C) Pacific Northwest National Laboratory 2015"
		  &= details
		  	 [ "`spectra-gen` is an executable that lets you generate frequency-dependent 1H FT-NMR spectra."
			 , ""
			 , "NUCLEI"
			 , ""
			 , "  Each nucleus is specified by an initial amplitude [1], a signal frequency [Hz], a relaxation time constant [s] and a signal phase [rad]."
			 , ""
			 , "SCANS"
			 , ""
			 , "  Each scan is specified by a number of acquisitions (power of two), the error in quad phase shift [rad], the imbalance in imaginary amplifier gain [%], DC in real channel [%], DC in imaginary channel [%], the Gaussian noise level [1] and the exponential apodization line broadening factor [Hz]."
			 , ""
			 , "EXAMPLES"
			 , ""
			 , "  Generate eight FT-NMR spectra for two different protons, one at 20Hz and the other at 30Hz, with a Gaussian noise level of 0.2 and an exponential apodization line broadening factor of 1 Hz."
			 , ""
			 , "    spectra-gen \\"
			 , "      --data-points=9 --dwell-time=0.01 --zero-fills=1 \\"
			 , "      --nucleus=1,20,1,0 --nucleus=1,30,1,0 \\"
			 , "      --scan=3,0,0,0,0,0.2,1"
			 , ""
			 ]

validateSpectraGen :: (Num a, Ord a) => SpectraGen a -> [String]
validateSpectraGen args = execWriter m
	where
		m :: (MonadWriter [String] m) => m ()
		m = do
			when (_spectraGenNumDataPointsPowerOfTwo args < 0) (Writer.tell ["`data-points` must be positive or zero."])
			when (_spectraGenNumZeroFills args < 0) (Writer.tell ["`zero-fills` must be positive or zero."])
			when (_spectraGenDwellTime args <= 0) (Writer.tell ["`dwell-time` must be positive and non-zero."])
			forM_ (zip ([1..] :: [Integer]) (_spectraGenNuclei args)) $ \(n, (alpha, _nu, t, _phi)) -> do
				when (alpha <= 0) (Writer.tell [printf "`nucleus.%d` -- initial amplitude (1st argument) must be positive and non-zero." n])
				-- nu
				when (t <= 0) (Writer.tell [printf "`nucleus.%d` -- relaxation time constant (3rd argument) must be positive and non-zero." n])
				-- phi
				return ()
			forM_ (zip ([1..] :: [Integer]) (_spectraGenScans args)) $ \(n, (numScansPowerOfTwo, _phi, g, reDC, imDC, sigma, lb)) -> do
				when (numScansPowerOfTwo < 0) (Writer.tell [printf "`scan.%d` -- number of scans (1st argument) must be positive or zero." n])
				-- phi
				unless ((g >= 0) && (g <= 1)) (Writer.tell [printf "`scan.%d` -- imbalance in imaginary amplifier gain (3rd argument) must be between zero and one inclusive." n])
				unless ((reDC >= 0) && (reDC <= 1)) (Writer.tell [printf "`scan.%d` -- DC in real channel (4th argument) must be between zero and one inclusive." n])
				unless ((imDC >= 0) && (imDC <= 1)) (Writer.tell [printf "`scan.%d` -- DC in imaginary channel (5th argument) must be between zero and one inclusive." n])
				when (sigma < 0) (Writer.tell [printf "`scan.%d` -- noise level (6th argument) must be positive or zero." n])
				when (lb < 0) (Writer.tell [printf "`scan.%d` -- line broadening factor (7th argument) must be positive or zero." n])
				return ()
			return ()

hPutCSVLn :: Handle -> [String] -> IO ()
hPutCSVLn handle xs = do
	forM_ (zip ([0..] :: [Integer]) xs) $ \ ~(n, x) -> do
		unless (n == 0) (hPutChar handle ',')
		hPutStr handle x
	hPutChar handle '\n'

hPutCSVHeader :: (MonadIO m, MonadReader (Ideal a) m, Fractional a, Show a) => Handle -> m ()
hPutCSVHeader handle = Reader.asks (\ideal -> getFrequencyIndices (_idealDwellTime ideal) (2 ^ (_idealNumDataPointsPowerOfTwo ideal + _idealNumZeroFills ideal))) >>= liftIO . hPutCSVLn handle . map (show . getFrequency)

hPutCSVRow :: (MonadIO m, MonadReader (Ideal a) m, RealFloat a, Show a, Storable a) => Handle -> Spectrum Frequency m a -> m ()
hPutCSVRow handle frequencySpectrum = fromFrequencySpectrum frequencySpectrum >>= liftIO . hPutCSVLn handle . map (show . getDimensionless . realPart . snd)

type Quad m a = (Tagged RealPart (Spectrum Frequency m a), Tagged RealPart (Spectrum Frequency m a), Tagged ImagPart (Spectrum Frequency m a), Tagged ImagPart (Spectrum Frequency m a))

main :: IO ()
main = do
	args <- cmdArgs (def :: SpectraGen Double)
	
	let errors = validateSpectraGen args
	
	if null errors
		then do
			let
				ideal :: Ideal Double
				ideal = Ideal
					{ _idealNumDataPointsPowerOfTwo = _spectraGenNumDataPointsPowerOfTwo args
					, _idealNumZeroFills = _spectraGenNumZeroFills args
					, _idealDwellTime = _spectraGenDwellTime args
					}

				nuclei :: [Nucleus Double]
				nuclei = map (\ ~(alpha, nu, t, phi) -> Nucleus
					{ _nucleusInitialAmplitude = alpha
					, _nucleusFrequency = nu
					, _nucleusDecayTimeUnit = t
					, _nucleusPhaseAngle = phi
					}) (_spectraGenNuclei args)
		
				realChannelP :: (MonadReader (Ideal Double) m, MonadState StdGen m) => Producer (Tagged RealPart (Channel m Double)) m ()
				realChannelP = forM_ (_spectraGenScans args) $ \ ~(numScansPowerOfTwo, phi, g, reDC, imDC, sigma, lb) -> do
			
					let
						nonIdeal :: NonIdeal Double
						nonIdeal = NonIdeal
							{ _nonIdealTransmitterPhase = 0
							, _nonIdealQuadPhaseShiftError = phi
							, _nonIdealAmplifierGainImbalance = g
							, _nonIdealRealChannelDC = reDC
							, _nonIdealImagChannelDC = imDC
							}

					replicateM_ (2 ^ numScansPowerOfTwo) (yield (fmap (withApodization (ExpApodization lb) . withNoise (GaussianNoise 0 sigma)) (realChannel nonIdeal nuclei)))
		
				imagChannelP :: (MonadReader (Ideal Double) m, MonadState StdGen m) => Producer (Tagged ImagPart (Channel m Double)) m ()
				imagChannelP = forM_ (_spectraGenScans args) $ \ ~(numScansPowerOfTwo, phi, g, reDC, imDC, sigma, lb) -> do
			
					let
						nonIdeal :: NonIdeal Double
						nonIdeal = NonIdeal
							{ _nonIdealTransmitterPhase = 0
							, _nonIdealQuadPhaseShiftError = phi
							, _nonIdealAmplifierGainImbalance = g
							, _nonIdealRealChannelDC = reDC
							, _nonIdealImagChannelDC = imDC
							}

					replicateM_ (2 ^ numScansPowerOfTwo) (yield (fmap (withApodization (ExpApodization lb) . withNoise (GaussianNoise 0 sigma)) (imagChannel nonIdeal nuclei)))

			runReaderT (hPutCSVHeader stdout) ideal

			g0 <- getStdGen
			((), g1, ()) <- runRWST (runEffect (P.zip (realChannelP >-> P.map (fmap fromChannel) >-> P.map (fmap dft)) (imagChannelP >-> P.map (fmap fromChannel) >-> P.map (fmap dft)) >-> P.map (\ ~(re, im) -> (realQuadL re im, realQuadR re im, imagQuadL im re, imagQuadR im re)) >-> P.map (\ ~(reL, reR, imL, imR) -> (fmap normalize reL, fmap normalize reR, fmap normalize imL, fmap normalize imR)) >-> P.chain (\ ~(_reL, reR, _imL, _imR) -> hPutCSVRow stdout (unTagged reR)) >-> P.drain)) ideal g0
			setStdGen g1

			exitSuccess
		else do
			forM_ errors $ \msg ->
				hPutStrLn stderr msg

			exitFailure
