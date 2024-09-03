module ModelieroBase.Data.IpV6
  ( IpV6,
  )
where

import ModelieroBase.Classes
import ModelieroBase.Prelude
import Net.IPv6 qualified

newtype IpV6 = IpV6 Net.IPv6.IPv6
  deriving newtype (ToJSON, FromJSON)

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

instance IsString IpV6 where
  fromString = literalFromString

instance Show IpV6 where
  showsPrec = literalShowsPrec

instance Read IpV6 where
  readPrec = literalReadPrec
