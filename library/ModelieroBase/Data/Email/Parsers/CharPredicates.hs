module ModelieroBase.Data.Email.Parsers.CharPredicates where

import ModelieroBase.Prelude

localElement :: Char -> Bool
localElement x =
  isAlphaNum x || elem @[] x "!#$%&'*+/=?^_`{|}~-"

domainLabelHead :: Char -> Bool
domainLabelHead =
  isAlphaNum

domainLabelTail :: Char -> Bool
domainLabelTail x =
  isAlphaNum x || x == '-'
