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
    ViaLiteral (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.Types qualified as Aeson
import Data.Attoparsec.Text qualified as Attoparsec
import Language.Haskell.TH.Syntax qualified as Th
import ModelieroBase.Prelude
import Text.ParserCombinators.ReadPrec qualified as ReadPrec

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

-- |
-- Helper for defining 'Read' instances via 'Literal', which expect a Haskell string literal (in double quotes).
--
-- This is useful in combination with the 'IsString' instance which lets you implicitly construct from string literals and a 'Show' instance, which prints it in double quotes.
--
-- TODO: Provide a set of properties, which test this as a law. This will allow the user to test his/her instances.
literalReadPrec :: (Literal a) => ReadPrec.ReadPrec a
literalReadPrec =
  readPrec >>= either fail return . Attoparsec.parseOnly (literalParser <* Attoparsec.endOfInput)

-- |
-- Helper for defining 'Show' instances via 'Literal', producing a Haskell string literal (in double quotes).
literalShowsPrec :: (Literal a) => Int -> a -> ShowS
literalShowsPrec prec =
  showsPrec prec . literalToText

-- |
-- Helper for defining the 'IsString' instances to be able to define the literal directly as string.
--
-- WARNING: This function fails with an error when the value is not parsable.
literalFromString :: (Literal a) => String -> a
literalFromString string =
  string
    & fromString
    & literalEitherFromText
    & either (error . renderErrorReport) id
  where
    renderErrorReport reason =
      mconcat
        [ "Failed to parse string literal.\n\
          \  Reason: ",
          toList reason,
          "\n\
          \  Input: ",
          string
        ]

-- |
-- Helper for defining the 'ToJSON' instances via JSON String.
literalToJson :: (Literal a) => a -> Aeson.Value
literalToJson = Aeson.String . literalToText

-- |
-- Helper for defining the 'FromJSON' instances via JSON String.
literalParseJson :: (Literal a) => Aeson.Value -> Aeson.Parser a
literalParseJson json =
  Aeson.parseJSON json >>= either fail return . Attoparsec.parseOnly (literalParser <* Attoparsec.endOfInput)

-- * Deriving Via

-- |
-- DerivingVia helper.
--
-- Lets you derive all standard instances by only defining the 'Literal' instance explicitly.
newtype ViaLiteral a = AsLiteral {base :: a}

instance (Literal a) => Literal (ViaLiteral a) where
  literalParser = fmap AsLiteral literalParser
  literalToText = literalToText . (.base)

instance (Literal a) => IsString (ViaLiteral a) where
  fromString = literalFromString

instance (Literal a) => Read (ViaLiteral a) where
  readPrec = literalReadPrec

instance (Literal a) => Show (ViaLiteral a) where
  showsPrec = literalShowsPrec

instance (Literal a) => ToJSON (ViaLiteral a) where
  toJSON = literalToJson

instance (Literal a) => FromJSON (ViaLiteral a) where
  parseJSON = literalParseJson

instance (Literal a) => ToJSONKey (ViaLiteral a) where
  toJSONKey = Aeson.ToJSONKeyText toKey toEncoding
    where
      toKey = Aeson.Key.fromText . literalToText
      toEncoding = Aeson.Encoding.text . literalToText

instance (Literal a) => FromJSONKey (ViaLiteral a) where
  fromJSONKey = Aeson.FromJSONKeyTextParser \text ->
    text
      & literalEitherFromText
      & either
        (Aeson.parseFail . toList)
        return

instance (Literal a) => Hashable (ViaLiteral a) where
  hashWithSalt salt = hashWithSalt salt . literalToText

instance (Literal a) => Eq (ViaLiteral a) where
  (==) = on (==) literalToText

instance (Literal a) => Ord (ViaLiteral a) where
  compare = on compare literalToText
