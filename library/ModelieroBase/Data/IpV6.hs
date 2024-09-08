module ModelieroBase.Data.IpV6
  ( IpV6,
  )
where

import ModelieroBase.Classes
import ModelieroBase.ExtrasFor.Ip.Gens qualified as IpGens
import ModelieroBase.Prelude
import Net.IPv6 qualified

newtype IpV6 = IpV6 Net.IPv6.IPv6
  deriving newtype (Eq, Ord, Hashable, ToJSON, FromJSON)
  deriving
    (IsString, Show, Read, ToJSONKey, FromJSONKey)
    via (AsLiteral IpV6)

instance IsomorphicTo Net.IPv6.IPv6 IpV6 where
  to = coerce

instance IsomorphicTo IpV6 Net.IPv6.IPv6 where
  to = coerce

instance Special IpV6 where
  type GeneralizationOf IpV6 = Text
  type SpecializationErrorOf IpV6 = Text
  specialize = literalEitherFromText
  generalize = literalToText

instance Literal IpV6 where
  literalParser = coerce Net.IPv6.parser
  literalToText = coerce Net.IPv6.encode

instance Arbitrary IpV6 where
  arbitrary = coerce IpGens.ipV6

instance Anonymizable IpV6 where
  anonymize force (IpV6 base) =
    IpV6 (anonymize force base)
