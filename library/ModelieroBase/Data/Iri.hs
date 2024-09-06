module ModelieroBase.Data.Iri
  ( Iri,
  )
where

import Iri.Data qualified
import Iri.Parsing.Attoparsec.Text qualified
import Iri.Rendering.Text qualified
import ModelieroBase.Classes
import ModelieroBase.Prelude

newtype Iri = Iri Iri.Data.Iri
  deriving
    (IsString, Show, Read, ToJSON, FromJSON)
    via (AsLiteral Iri)

instance Special Iri where
  type GeneralizationOf Iri = Text
  type SpecializationErrorOf Iri = Text
  specialize = literalEitherFromText
  generalize = literalToText

instance Literal Iri where
  literalParser = coerce Iri.Parsing.Attoparsec.Text.iri
  literalToText = coerce Iri.Rendering.Text.iri
