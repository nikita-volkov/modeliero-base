module ModelieroBase.Proxies.ViaEnum
  ( ViaEnum (ViaEnum),
  )
where

import ModelieroBase.Classes
import ModelieroBase.Prelude

newtype ViaEnum a = ViaEnum a
  deriving newtype (Eq)

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
