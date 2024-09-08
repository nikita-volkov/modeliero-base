module ModelieroBase.Data.Hostname
  ( Hostname,
  )
where

import Iri.Data qualified
import Iri.Parsing.Attoparsec.Text qualified
import Iri.Rendering.Text qualified
import ModelieroBase.Classes
import ModelieroBase.ExtrasFor.Iri.Gens qualified as IriGens
import ModelieroBase.Prelude

newtype Hostname = Hostname Iri.Data.Host
  deriving newtype (Eq, Ord, Hashable)
  deriving
    (IsString, Show, Read, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
    via (AsLiteral Hostname)

instance Special Hostname where
  type GeneralizationOf Hostname = Text
  type SpecializationErrorOf Hostname = Text
  specialize = literalEitherFromText
  generalize = literalToText

instance Literal Hostname where
  literalParser = coerce Iri.Parsing.Attoparsec.Text.host
  literalToText = coerce Iri.Rendering.Text.host

instance Arbitrary Hostname where
  arbitrary = coerce IriGens.host

instance Anonymizable Hostname where
  anonymize force (Hostname iriHost) =
    iriHost
      & anonymize force
      & Hostname
