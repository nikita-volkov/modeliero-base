module ModelieroBase.Data.Iso8601Date
  ( Iso8601Date,
  )
where

import Data.Attoparsec.Time qualified
import Data.Time.ToText qualified
import ModelieroBase.Classes
import ModelieroBase.Prelude
import ModelieroBase.Proxies

-- | Canonical representation of ISO-8601 date values.
newtype Iso8601Date = Iso8601Date Day
  deriving newtype (Arbitrary, Eq, Ord)
  deriving
    (IsString, Show, Read, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
    via (ViaLiteral Iso8601Date)

instance Literal Iso8601Date where
  literalParser = fmap Iso8601Date Data.Attoparsec.Time.day
  literalToText = to . Data.Time.ToText.buildDay . coerce

instance IsomorphicTo Day Iso8601Date where
  to = coerce

instance IsomorphicTo Iso8601Date Day where
  to = coerce

instance Hashable Iso8601Date where
  hashWithSalt salt (Iso8601Date (ModifiedJulianDay integer)) =
    salt
      & flip hashWithSalt integer

instance Anonymizable Iso8601Date where
  anonymize = bool id anonymizeViaHashableAndArbitrary
