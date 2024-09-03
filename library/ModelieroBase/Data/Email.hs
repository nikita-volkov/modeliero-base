module ModelieroBase.Data.Email
  ( Email,
  )
where

import Data.Text.Encoding qualified
import ModelieroBase.Classes.Special
import ModelieroBase.Prelude
import Text.Email.Parser qualified
import Text.Email.Validate qualified

newtype Email = Email
  { base :: Text.Email.Parser.EmailAddress
  }

instance IsomorphicTo Text.Email.Parser.EmailAddress Email where
  to = coerce

instance IsomorphicTo Email Text.Email.Parser.EmailAddress where
  to = coerce

instance Special Email where
  type GeneralizationOf Email = Text
  type SpecializationErrorOf Email = Text
  specialize =
    bimap fromString Email
      . Text.Email.Validate.validate
      . Data.Text.Encoding.encodeUtf8
  generalize =
    Data.Text.Encoding.decodeUtf8Lenient
      . Text.Email.Validate.toByteString
      . (.base)
