module Data.Normalize where

-- | The `Normalize` class is used for types that can be normalized (adjusted to a notionally common scale).  Instances of the `Normalize` class should satisfy the following laws:
--
-- > normalize . normalize = normalize
--
class Normalize a where
	normalize :: a -> a
