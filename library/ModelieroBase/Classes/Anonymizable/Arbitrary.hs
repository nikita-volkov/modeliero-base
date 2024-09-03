module ModelieroBase.Classes.Anonymizable.Arbitrary where

import ModelieroBase.Prelude
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

fromInt :: (Arbitrary a) => Int -> a
fromInt seed =
  unGen arbitrary (mkQCGen seed) 0
