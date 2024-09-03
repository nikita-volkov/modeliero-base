module ModelieroBase.Classes.Anonymizable where

import ModelieroBase.Prelude

class Anonymizable a where
  anonymize :: a -> a

instance Anonymizable Text where
  anonymize =
    error "TODO"
