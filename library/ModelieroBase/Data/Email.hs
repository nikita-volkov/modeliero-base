module ModelieroBase.Data.Email
  ( Email,
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import Data.Text qualified as Text
import ModelieroBase.Classes
import ModelieroBase.Data.Email.Gens qualified as Gens
import ModelieroBase.Data.Email.Parsers qualified as Parsers
import ModelieroBase.Prelude
import ModelieroBase.Proxies
import Text.Builder qualified as TextBuilder

data Email = Email
  { local :: NonEmpty Text,
    domain :: NonEmpty Text
  }
  deriving stock (Eq, Ord)
  deriving
    (IsString, Show, Read, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
    via (ViaLiteral Email)

instance Literal Email where
  literalParser = do
    local <- Parsers.local Attoparsec.<?> "local"
    _ <- Attoparsec.char '@'
    domain <- Parsers.domain Attoparsec.<?> "domain"
    pure Email {..}
  literalToText Email {..} =
    mconcat
      [ case local of
          localHead :| localTail ->
            TextBuilder.text localHead
              <> foldMap (mappend "." . TextBuilder.text) localTail,
        "@",
        case domain of
          domainHead :| domainTail ->
            TextBuilder.text domainHead
              <> foldMap (mappend "." . TextBuilder.text) domainTail
      ]
      & TextBuilder.run

instance Hashable Email where
  hashWithSalt salt Email {..} =
    salt
      & flip hashWithSalt local
      & flip hashWithSalt domain

instance Arbitrary Email where
  arbitrary =
    Email <$> Gens.local <*> Gens.domain

instance Anonymizable Email where
  anonymize = bool id go
    where
      go Email {..} =
        Email
          { local =
              local
                & toList
                & Text.intercalate "."
                & anonymizeText maxBound
                & pure,
            domain =
              domain
                & toList
                & Text.intercalate "."
                & anonymizeText maxBound
                & pure
          }
