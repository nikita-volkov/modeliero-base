module ModelieroBase.Data.Iso8601DateTime
  ( Iso8601DateTime,
  )
where

import Data.Attoparsec.Time qualified
import Data.Time.ToText qualified
import ModelieroBase.Classes
import ModelieroBase.Prelude
import ModelieroBase.Proxies

-- | Canonical representation of ISO-8601 date-time values.
newtype Iso8601DateTime = Iso8601DateTime ZonedTime
  deriving newtype (Arbitrary)
  deriving
    (IsString, Show, Read, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
    via (ViaLiteral Iso8601DateTime)

instance Literal Iso8601DateTime where
  literalParser = coerce Data.Attoparsec.Time.zonedTime
  literalToText = to . Data.Time.ToText.buildZonedTime . coerce

instance IsomorphicTo ZonedTime Iso8601DateTime where
  to = coerce

instance IsomorphicTo Iso8601DateTime ZonedTime where
  to = coerce

instance Eq Iso8601DateTime where
  Iso8601DateTime l == Iso8601DateTime r =
    (zonedTimeToLocalTime l == zonedTimeToLocalTime r)
      && (zonedTimeZone l == zonedTimeZone r)

instance Ord Iso8601DateTime where
  Iso8601DateTime l <= Iso8601DateTime r =
    (zonedTimeToLocalTime l <= zonedTimeToLocalTime r)
      && (zonedTimeZone l <= zonedTimeZone r)

instance Hashable Iso8601DateTime where
  hashWithSalt
    salt
    ( Iso8601DateTime
        ( ZonedTime
            ( LocalTime
                (ModifiedJulianDay day)
                (TimeOfDay hour minute second)
              )
            (TimeZone timeZoneMinutes summerOnly timeZoneName)
          )
      ) =
      salt
        & flip hashWithSalt day
        & flip hashWithSalt hour
        & flip hashWithSalt minute
        & flip hashWithSalt second
        & flip hashWithSalt timeZoneMinutes
        & flip hashWithSalt summerOnly
        & flip hashWithSalt timeZoneName

instance Anonymizable Iso8601DateTime where
  anonymize = bool id anonymizeViaHashableAndArbitrary
