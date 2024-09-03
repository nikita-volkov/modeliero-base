module ModelieroBase.Data.IpV6
  ( IpV6,
  )
where

import ModelieroBase.Classes.Special
import ModelieroBase.Prelude
import Net.IPv6 qualified

newtype IpV6 = IpV6
  { base :: Net.IPv6.IPv6
  }
  deriving newtype (ToJSON, FromJSON)

instance IsomorphicTo Net.IPv6.IPv6 IpV6 where
  to = coerce

instance IsomorphicTo IpV6 Net.IPv6.IPv6 where
  to = coerce

instance Special IpV6 where
  type GeneralizationOf IpV6 = Text
  type SpecializationErrorOf IpV6 = ()
  specialize =
    maybe (Left ()) (Right . IpV6) . Net.IPv6.decode
  generalize =
    Net.IPv6.encode . (.base)

instance IsString IpV6 where
  fromString string =
    string
      & fromString
      & Net.IPv6.decode
      & fmap IpV6
      & fromMaybe (error errorMessage)
    where
      errorMessage =
        "Invalid IP: " <> show string

instance Show IpV6 where
  show = show . generalize
