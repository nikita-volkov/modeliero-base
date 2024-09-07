module ModelieroBase.Data.Email
  ( Email,
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import ModelieroBase.Classes
import ModelieroBase.Data.Email.Attoparsec qualified as AttoparsecHelpers
import ModelieroBase.Prelude
import Text.Builder qualified as TextBuilder

data Email = Email
  { local :: NonEmpty Text,
    domain :: NonEmpty Text
  }
  deriving
    (IsString, Show, Read, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
    via (AsLiteral Email)

instance Literal Email where
  literalParser = do
    local <- AttoparsecHelpers.local Attoparsec.<?> "local"
    _ <- Attoparsec.char '@'
    domain <- AttoparsecHelpers.domain Attoparsec.<?> "domain"
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
