-- |
-- Common interface for custom data-types which can be represented with a custom textual literal.
--
-- Provides a reusable compile-time constructor, parsing and rendering APIs.
module ModelieroBase.Classes.Literal
  ( Literal (..),
    literalSplice,
    literalEitherFromText,
    literalMaybeFromText,
    literalizedPrism,
  )
where

import Data.Attoparsec.Text qualified as Attoparsec
import Language.Haskell.TH.Syntax qualified as Th
import ModelieroBase.Prelude

-- | Value that has a canonical textual representation.
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
  literalToText :: a -> Text

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
literalSplice :: (Literal a, Th.Lift a) => String -> Th.Code Th.Q a
literalSplice literal = Th.Code do
  literal <- case literalEitherFromText (fromString literal) of
    Right literal -> return literal
    Left err -> fail $ to err
  Th.examineCode $ Th.liftTyped literal

literalEitherFromText :: (Literal a) => Text -> Either Text a
literalEitherFromText =
  first fromString
    . Attoparsec.parseOnly (literalParser <* Attoparsec.endOfInput)

literalMaybeFromText :: (Literal a) => Text -> Maybe a
literalMaybeFromText =
  either (const Nothing) Just
    . literalEitherFromText

-- |
-- Prism from 'Text' to its structured view via 'Literal'.
--
-- Useful for editing textual values.
--
-- Provides a way to access and modify a textual value by focusing on its structured view when it\'s possible.
--
-- It\'s Van Laarhoven style, so it\'s compatible with all the optics libraries.
literalizedPrism ::
  (Choice p, Applicative f, Literal literal) =>
  p literal (f literal) ->
  p Text (f Text)
literalizedPrism = prism' literalToText literalMaybeFromText
