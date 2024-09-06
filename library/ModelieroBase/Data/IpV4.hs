module ModelieroBase.Data.IpV4
  ( IpV4,
  )
where

import ModelieroBase.Classes
import ModelieroBase.Prelude
import Net.IPv4 qualified

newtype IpV4 = IpV4 Net.IPv4.IPv4
  deriving newtype (ToJSON, FromJSON)
  deriving
    (IsString, Show, Read)
    via (AsLiteral IpV4)

instance IsomorphicTo Net.IPv4.IPv4 IpV4 where
  to = coerce

instance IsomorphicTo IpV4 Net.IPv4.IPv4 where
  to = coerce

instance Special IpV4 where
  type GeneralizationOf IpV4 = Text
  type SpecializationErrorOf IpV4 = Text
  specialize = literalEitherFromText
  generalize = literalToText

instance Literal IpV4 where
  literalParser = coerce Net.IPv4.parser
  literalToText = coerce Net.IPv4.encode
