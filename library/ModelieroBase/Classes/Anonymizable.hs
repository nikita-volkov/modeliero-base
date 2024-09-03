module ModelieroBase.Classes.Anonymizable where

import ModelieroBase.Classes.Anonymizable.Arbitrary qualified as Arbitrary
import ModelieroBase.Prelude

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

instance (Anonymizable a, Anonymizable b) => Anonymizable (Either a b) where
  anonymize total = bimap (anonymize total) (anonymize total)

instance (Anonymizable a, Anonymizable b) => Anonymizable (a, b) where
  anonymize total = bimap (anonymize total) (anonymize total)

instance (Anonymizable a, Anonymizable b, Anonymizable c) => Anonymizable (a, b, c) where
  anonymize total (a, b, c) =
    (anonymize total a, anonymize total b, anonymize total c)

instance Anonymizable Text where
  anonymize = bool id anonymizeViaHashableArbitrary

-- |
-- Hash the value into 64-bits,
-- supply that hash as the seed for the value generator provided by the Arbitrary instance.
anonymizeViaHashableArbitrary :: (Hashable a, Arbitrary a) => a -> a
anonymizeViaHashableArbitrary =
  Arbitrary.fromInt . hash

-- |
-- Hash the value into 64-bits,
-- apply the provided modulo to reduce the range of the produced hashes,
-- supply that hash as the seed for the value generator provided by the Arbitrary instance.
anonymizeViaHashableArbitraryWithMod :: (Hashable a, Arbitrary a) => Int -> a -> a
anonymizeViaHashableArbitraryWithMod x =
  Arbitrary.fromInt . mod x . hash
