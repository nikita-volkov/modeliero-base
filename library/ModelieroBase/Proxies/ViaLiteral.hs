module ModelieroBase.Proxies.ViaLiteral
  ( ViaLiteral (ViaLiteral),
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.Types qualified as Aeson
import Data.Attoparsec.Text qualified as Attoparsec
import ModelieroBase.Classes
import ModelieroBase.Prelude
import Text.ParserCombinators.ReadPrec qualified as ReadPrec

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

-- |
-- DerivingVia helper.
--
-- Lets you derive all standard instances by only defining the 'Literal' instance explicitly.
newtype ViaLiteral a = ViaLiteral {base :: a}

instance (Literal a) => Literal (ViaLiteral a) where
  literalParser = fmap ViaLiteral literalParser
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
