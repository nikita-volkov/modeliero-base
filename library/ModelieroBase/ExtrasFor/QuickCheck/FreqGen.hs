module ModelieroBase.ExtrasFor.QuickCheck.FreqGen
  ( compile,
    FreqGen,
    singleton,
    set,
    charRange,
  )
where

import ModelieroBase.Prelude hiding (singleton)
import Test.QuickCheck.Gen

compile :: FreqGen a -> Gen a
compile (FreqGen list) =
  frequency list

newtype FreqGen a
  = FreqGen [(Int, Gen a)]
  deriving (Functor)
  deriving newtype (Semigroup, Monoid)

singleton :: Int -> Gen a -> FreqGen a
singleton freq gen =
  FreqGen [(freq, gen)]

set :: [a] -> FreqGen a
set list =
  singleton
    (length list)
    (elements list)

charRange :: Char -> Char -> FreqGen Char
charRange start end =
  singleton
    (ord end - ord start)
    (choose (start, end))
