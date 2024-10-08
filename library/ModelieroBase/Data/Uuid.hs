module ModelieroBase.Data.Uuid
  ( Uuid,
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import Data.UUID qualified as Uuid
import ModelieroBase.Classes
import ModelieroBase.Prelude
import ModelieroBase.Proxies

newtype Uuid = Uuid Uuid.UUID
  deriving newtype
    (Eq, Ord, Hashable, Arbitrary)
  deriving
    (IsString, Show, Read, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
    via (ViaLiteral Uuid)

instance Literal Uuid where
  literalParser = coerce do
    text <- Attoparsec.take 36
    case Uuid.fromText text of
      Just uuid -> return uuid
      Nothing -> fail (showString "Unparsable UUID: " (show text))
  literalToText = coerce Uuid.toText

instance Anonymizable Uuid where
  anonymize = bool id \(Uuid base) ->
    base
      & anonymizeViaHashableAndArbitrary
      & Uuid

instance IsomorphicTo Uuid.UUID Uuid where
  to = coerce

instance IsomorphicTo Uuid Uuid.UUID where
  to = coerce
