module ModelieroBase.Data.Hostname
  ( Hostname,
  )
where

import Iri.Data qualified
import Iri.Parsing.Attoparsec.Text qualified
import Iri.Rendering.Text qualified
import ModelieroBase.Classes
import ModelieroBase.Prelude

newtype Hostname = Hostname Iri.Data.Host

instance Special Hostname where
  type GeneralizationOf Hostname = Text
  type SpecializationErrorOf Hostname = Text
  specialize = literalEitherFromText
  generalize = literalToText

instance Literal Hostname where
  literalParser = coerce Iri.Parsing.Attoparsec.Text.host
  literalToText = coerce Iri.Rendering.Text.host

instance IsString Hostname where
  fromString = literalFromString

instance Show Hostname where
  showsPrec = literalShowsPrec

instance Read Hostname where
  readPrec = literalReadPrec
