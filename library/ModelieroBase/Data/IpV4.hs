module ModelieroBase.Data.IpV4
  ( IpV4,
  )
where

import ModelieroBase.Classes.Literal
import ModelieroBase.Classes.Special
import ModelieroBase.Prelude
import Net.IPv4 qualified

newtype IpV4 = IpV4
  { base :: Net.IPv4.IPv4
  }
  deriving newtype (ToJSON, FromJSON)

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
  literalParser = fmap IpV4 Net.IPv4.parser
  literalToText = Net.IPv4.encode . (.base)

instance IsString IpV4 where
  fromString string =
    string
      & Net.IPv4.decodeString
      & fmap IpV4
      & fromMaybe (error errorMessage)
    where
      errorMessage =
        "Invalid IP: " <> show string

instance Show IpV4 where
  show = show . generalize
