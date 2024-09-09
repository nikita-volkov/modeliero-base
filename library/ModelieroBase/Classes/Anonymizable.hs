module ModelieroBase.Classes.Anonymizable where

import Crypto.Hash.SHA256 qualified
import Data.Base64.Types qualified
import Data.ByteString.Base64.URL qualified
import Data.Text qualified as Text
import Data.Text.Encoding qualified
import Iri.Data qualified
import ModelieroBase.Classes.Anonymizable.Arbitrary qualified as Arbitrary
import ModelieroBase.Prelude
import Net.IPv4 qualified
import Net.IPv6 qualified
import Ptr.ByteString qualified
import Ptr.Peek qualified
import Ptr.Poking qualified

-- | Structure-preserving anonymization.
class Anonymizable a where
  anonymize ::
    -- | Whether total anonymization is required.
    -- Otherwise it will be decided by the instance at which parts it needs to be applied.
    --
    -- This allows to both control which parts of the data-structure must be anonymized and to override that at the instance level.
    --
    -- E.g., if you define a domain type 'CustomerEmail' you may want to disregard this flag and anonymize always.
    -- However for general types like 'Text' the anonymization will only be applied when this flag is 'True'.
    Bool ->
    a ->
    a

instance (Anonymizable a) => Anonymizable (Maybe a) where
  anonymize total = fmap (anonymize total)

instance (Anonymizable a, Anonymizable b) => Anonymizable (Either a b) where
  anonymize total = bimap (anonymize total) (anonymize total)

instance (Anonymizable a, Anonymizable b) => Anonymizable (a, b) where
  anonymize total = bimap (anonymize total) (anonymize total)

instance (Anonymizable a, Anonymizable b, Anonymizable c) => Anonymizable (a, b, c) where
  anonymize total (a, b, c) =
    (anonymize total a, anonymize total b, anonymize total c)

instance Anonymizable Text where
  anonymize = bool id (anonymizeText maxBound)

instance Anonymizable Bool where
  anonymize = bool id (const True)

instance Anonymizable Int8 where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Int8 (div maxBound 2)))

instance Anonymizable Int16 where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Int16 (div maxBound 2)))

instance Anonymizable Int32 where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Int32 (div maxBound 2)))

instance Anonymizable Int64 where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Int64 (div maxBound 2)))

instance Anonymizable Int where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Int (div maxBound 2)))

instance Anonymizable Word8 where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Word8 (div maxBound 2)))

instance Anonymizable Word16 where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Word16 (div maxBound 2)))

instance Anonymizable Word32 where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Word32 (div maxBound 2)))

instance Anonymizable Word64 where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Word64 (div maxBound 2)))

instance Anonymizable Word where
  anonymize = bool id (anonymizeViaHashableAndArbitraryWithMod (fromIntegral @Word (div maxBound 2)))

instance Anonymizable Net.IPv4.IPv4 where
  anonymize = bool id go
    where
      go (Net.IPv4.toOctets -> (a, b, c, d)) =
        Net.IPv4.fromOctets
          (anonymize True a)
          (anonymize True b)
          (anonymize True c)
          (anonymize True d)

instance Anonymizable Net.IPv6.IPv6 where
  anonymize = bool id go
    where
      go (Net.IPv6.toWord32s -> (a, b, c, d)) =
        Net.IPv6.fromWord32s
          (anonymize True a)
          (anonymize True b)
          (anonymize True c)
          (anonymize True d)

instance Anonymizable Iri.Data.DomainLabel where
  anonymize = bool id go
    where
      go (Iri.Data.DomainLabel text) =
        text
          & anonymizeText maxBound
          & Iri.Data.DomainLabel

instance Anonymizable Iri.Data.RegName where
  anonymize = bool id go
    where
      go (Iri.Data.RegName labels) =
        labels
          & toList
          & fmap (\(Iri.Data.DomainLabel text) -> text)
          & Text.intercalate "."
          & anonymizeText maxBound
          & Iri.Data.DomainLabel
          & pure
          & Iri.Data.RegName

instance Anonymizable Iri.Data.Host where
  anonymize force = \case
    Iri.Data.NamedHost regName ->
      regName
        & anonymize force
        & Iri.Data.NamedHost
    Iri.Data.IpV4Host ipV4 ->
      ipV4
        & anonymize force
        & Iri.Data.IpV4Host
    Iri.Data.IpV6Host ipV6 ->
      ipV6
        & anonymize force
        & Iri.Data.IpV6Host

-- |
-- Anonymize text controlling the cardinality (maximum amount of possible variations).
anonymizeText :: Int -> Text -> Text
anonymizeText cardinality text =
  text
    & Data.Text.Encoding.encodeUtf8
    & Crypto.Hash.SHA256.hash
    & flip Ptr.ByteString.peek Ptr.Peek.leWord64
    & fromMaybe 0
    & flip mod (fromIntegral cardinality)
    & Ptr.Poking.leWord64
    & Ptr.ByteString.poking
    & Data.ByteString.Base64.URL.encodeBase64Unpadded
    & Data.Base64.Types.extractBase64

-- |
-- Hash the value into 64-bits,
-- supply that hash as the seed for the value generator provided by the Arbitrary instance.
anonymizeViaHashableAndArbitrary :: (Hashable a, Arbitrary a) => a -> a
anonymizeViaHashableAndArbitrary =
  Arbitrary.fromInt . hash

-- |
-- Hash the value into 64-bits,
-- apply the provided modulo to reduce the cardinality of the produced hashes,
-- supply that hash as the seed for the value generator provided by the Arbitrary instance.
anonymizeViaHashableAndArbitraryWithMod :: (Hashable a, Arbitrary a) => Int -> a -> a
anonymizeViaHashableAndArbitraryWithMod x =
  Arbitrary.fromInt . mod x . hash
