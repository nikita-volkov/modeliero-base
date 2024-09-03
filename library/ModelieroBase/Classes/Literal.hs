-- |
-- Common interface for custom data-types which can be represented with a custom textual literal.
--
-- Provides a reusable compile-time constructor, parsing and rendering APIs.
module ModelieroBase.Classes.Literal where

import Data.Attoparsec.Text qualified as Attoparsec
import Language.Haskell.TH.Syntax qualified as Th
import ModelieroBase.Prelude
import Text.Builder qualified as TextBuilder

-- | Value that has a textual representation.
--
-- This class is lawful: rendering the value and then parsing it
-- should succeed and produce the original value.
--
-- The law can be represented with the following code:
-- @
-- Right a
--   == literalFromText (literalToText a)
-- @
class Literal a where
  literalParser :: Attoparsec.Parser a
  literalTextBuilder :: a -> TextBuilder.Builder

-- | If a literal can also be converted to code, we can instantiate and
-- validate it at compile time, thus letting us provide guarantees that a
-- value constructed this way is correct.
--
-- Examples of what kinds of literals there may be:
--
-- > path :: Path
-- > path = $$(literalSplice "/usr/local/bin")
--
-- > url :: Url
-- > url = $$(literalSplice "http://example.org")
--
-- > email :: Email
-- > email = $$(literalSplice "user@example.org")
--
-- > phoneNumber :: PhoneNumber
-- > phoneNumber = $$(literalSplice "+74950123456")
--
-- > samePhoneNumber :: PhoneNumber
-- > samePhoneNumber = $$(literalSplice "+7(495)012-3456")
--
-- > uuid :: Uuid
-- > uuid = $$(literalSplice "123e4567-e89b-12d3-a456-426614174000")
--
-- > -- | Construct text not by converting from a string literal,
-- > -- but by packing a byte-array literal, which may be more efficient.
-- > text :: Text
-- > text = $$(literalSplice "Example text")
literalSplice :: (Literal a, Th.Lift a) => String -> Th.Code Th.Q a
literalSplice literal = Th.Code do
  literal <- case literalEitherFromText (fromString literal) of
    Right literal -> return literal
    Left err -> fail $ to err
  Th.examineCode $ Th.liftTyped literal

literalToText :: (Literal a) => a -> Text
literalToText = TextBuilder.run . literalTextBuilder

literalEitherFromText :: (Literal a) => Text -> Either Text a
literalEitherFromText =
  first fromString
    . Attoparsec.parseOnly (literalParser <* Attoparsec.endOfInput)

literalMaybeFromText :: (Literal a) => Text -> Maybe a
literalMaybeFromText =
  either (const Nothing) Just
    . literalEitherFromText
