module ModelieroBase.Proxies.ViaEnum
  ( ViaEnum (ViaEnum),
  )
where

import ModelieroBase.Classes
import ModelieroBase.Prelude
import Test.QuickCheck.Arbitrary qualified as QuickCheck.Arbitrary
import Test.QuickCheck.Gen qualified as QuickCheck.Gen

newtype ViaEnum a = ViaEnum a
  deriving newtype (Eq, Enum, Bounded)

instance (Enum a, Eq a) => Hashable (ViaEnum a) where
  hashWithSalt salt (ViaEnum x) =
    x
      & fromEnum
      & hashWithSalt salt

instance (Enum a, Bounded a) => Anonymizable (ViaEnum a) where
  anonymize = \case
    False -> id
    True -> \(ViaEnum value) ->
      let index = fromEnum value
          newIndex = mod index anonymizedSize
          size = succ maxIndex
          anonymizedSize =
            case div size 2 of
              0 -> 1
              x -> x
          maxIndex =
            maxBound
              & asTypeOf value
              & fromEnum
          newValue = toEnum newIndex
       in ViaEnum newValue

instance (Enum a, Bounded a) => QuickCheck.Arbitrary.Arbitrary (ViaEnum a) where
  arbitrary =
    QuickCheck.Gen.chooseEnum (minBound, maxBound)
