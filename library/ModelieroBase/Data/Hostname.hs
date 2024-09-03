module ModelieroBase.Data.Hostname
  ( Hostname,
  )
where

import Iri.Data qualified
import Iri.Parsing.Text qualified
import Iri.Rendering.Text qualified
import ModelieroBase.Classes.Special
import ModelieroBase.Prelude

newtype Hostname = Hostname
  { base :: Iri.Data.Host
  }

instance Special Hostname where
  type GeneralizationOf Hostname = Text
  type SpecializationErrorOf Hostname = Text
  specialize =
    fmap Hostname . Iri.Parsing.Text.host
  generalize =
    Iri.Rendering.Text.host . (.base)

instance IsString Hostname where
  fromString input =
    input
      & fromString
      & Iri.Parsing.Text.host
      & either (error . renderError) Hostname
    where
      renderError reason =
        mconcat
          [ "Failed to parse.\n\
            \  Reason: ",
            toList reason,
            "\n\
            \  Input: ",
            input
          ]
