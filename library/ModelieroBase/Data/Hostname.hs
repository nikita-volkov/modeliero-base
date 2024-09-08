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
  anonymize force (Hostname iriHost) = case iriHost of
    Iri.Data.NamedHost iriRegName ->
      iriRegName
        & error "TODO"
    Iri.Data.IpV4Host ipV4 ->
      ipV4
        & anonymize force
        & Iri.Data.IpV4Host
        & Hostname
    Iri.Data.IpV6Host ipV6 ->
      ipV6
        & anonymize force
        & Iri.Data.IpV6Host
        & Hostname
