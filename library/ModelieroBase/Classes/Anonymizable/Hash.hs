module ModelieroBase.Classes.Anonymizable.Hash where

import Crypto.Hash.SHA256 qualified
import ModelieroBase.Prelude
import Ptr.ByteString qualified
import Ptr.Peek qualified

fromByteString :: ByteString -> Word64
fromByteString input =
  input
    & Crypto.Hash.SHA256.hash
    & flip Ptr.ByteString.peek Ptr.Peek.leWord64
    & fromMaybe 0

-- | Remap the hash into a space determined by the provided maximum value.
resize ::
  -- | Maximum.
  Word64 ->
  -- | Hash.
  Word64 ->
  -- | Remapped hash.
  Word64
resize =
  flip mod
