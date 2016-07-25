module NMR.Util
( -- * Calculated parameters
  getAcquisitionTime , getDigitalResolution , getSamplingRate , getDwellTime , getSpectralWindow
  -- * Chemical shift
, toChemicalShift , fromChemicalShift
  -- * Indices
, getTimeIndices , getFrequencyIndices
) where

import NMR.Quantity

-- | Takes a dwell time /dw/ and a number of data points /n/, and returns the total amount of time that the data is sampled (acquisition time).
--
getAcquisitionTime :: (Num a) => Time a -> Int -> Time a
getAcquisitionTime dw = (* dw) . fromInteger . toInteger

-- | Takes a dwell time /dw/ and a number of data points /n/, and returns the spacing between each data point (digital resolution).
--
getDigitalResolution :: (Fractional a) => Time a -> Int -> Frequency a
getDigitalResolution dw = Frequency . recip . getTime . getAcquisitionTime dw

-- | Takes a dwell time /dw/, and returns the frequency at which the instrument samples the signal (sampling rate).
--
getSamplingRate :: (Fractional a) => Time a -> Frequency a
getSamplingRate = Frequency . recip . getTime

-- | Takes a spectral window /sw/, and returns the time between each sample (dwell time).
--
getDwellTime :: (Fractional a) => Frequency a -> Time a
getDwellTime = Time . recip . (2 *) . getFrequency

-- | Takes a dwell time /dw/, and returns the upper bound of the range of frequencies that can be sampled (spectral window).
--
getSpectralWindow :: (Fractional a) => Time a -> Frequency a
getSpectralWindow = Frequency . recip . (2 *) . getTime

-- | Takes a reference frequency /ref/ and a measured frequency /v/, and returns the chemical shift.
--
toChemicalShift :: (Fractional a) => Frequency a -> Frequency a -> Dimensionless a
toChemicalShift (Frequency ref) (Frequency v) = Dimensionless ((v - ref) / ref)
-- toChemicalShift (Frequency ref) (Frequency v) = Dimensionless (((v - ref) / ref) * 10e6)

-- | Takes a reference frequency /ref/ and a chemical shift /ppm/, and returns the measured frequency.
--
fromChemicalShift :: (Fractional a) => Frequency a -> Dimensionless a -> Frequency a
fromChemicalShift (Frequency ref) (Dimensionless ppm) = Frequency ((ppm * ref) + ref)
-- fromChemicalShift (Frequency ref) (Dimensionless ppm) = Frequency (((ppm * ref) / 10e6) + ref)

-- | Takes a dwell time /dw/ and a number of data points /n/, and returns a list of time-domain indices.
--
getTimeIndices :: (Num a) => Time a -> Int -> [Time a]
getTimeIndices dw n = map (getAcquisitionTime dw) [0..(n - 1)]

-- | Takes a dwell time /dw/ and a number of data points /n/, and returns a list of frequency-domain indices.
--
getFrequencyIndices :: (Fractional a) => Time a -> Int -> [Frequency a]
getFrequencyIndices dw n = map (Frequency . (/ getTime (getAcquisitionTime dw n)) . fromInteger . toInteger . subtract (n `div` 2)) [0..(n - 1)]
