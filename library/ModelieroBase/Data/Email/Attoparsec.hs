module ModelieroBase.Data.Email.Attoparsec where

import Data.Attoparsec.Text
import ModelieroBase.Data.Email.Attoparsec.CharPredicates qualified as CharPredicates
import ModelieroBase.Prelude

sepBy1Ne :: (Alternative f) => f a -> f s -> f (NonEmpty a)
sepBy1Ne p s =
  (:|) <$> p <*> (s *> sepBy p s)

local :: Parser (NonEmpty Text)
local = do
  (firstElement, size) <- localElement 0
  go size \tail -> firstElement :| tail
  where
    go !size !finalize = do
      dot <- char '.' $> True <|> pure False
      if dot
        then do
          (!element, size) <- localElement (succ size)
          if size == 64
            then do
              peekChar >>= \case
                Just '@' -> pure (finalize [element])
                Nothing -> fail "Not enough input"
                Just _ -> fail "Local part has run to more than 64 chars, which is the maximum as per RFC-3696"
            else go size \tail -> finalize (element : tail)
        else pure (finalize [])

localElement :: Int -> Parser (Text, Int)
localElement size =
  runScanner size \size char ->
    if CharPredicates.localElement char && size < 64
      then Just (succ size)
      else Nothing

domain :: Parser (NonEmpty Text)
domain = do
  (head, size) <- domainLabel 0
  go size \tail -> head :| tail
  where
    go !size !finalize = do
      dot <- char '.' $> True <|> pure False
      if dot
        then do
          (!label, size) <- domainLabel (succ size)
          if size == 64
            then do
              peekChar >>= \case
                Just '@' -> pure (finalize [label])
                Nothing -> fail "Not enough input"
                Just _ -> fail "Local part has run to more than 64 chars, which is the maximum as per RFC-3696"
            else go size \tail -> finalize (label : tail)
        else pure (finalize [])

domainLabel :: Int -> Parser (Text, Int)
domainLabel domainSize = do
  firstChar <- satisfy CharPredicates.domainLabelHead
  go domainSize (0 :: Int) [firstChar]
  where
    go domainSize labelSize chars =
      optional (satisfy CharPredicates.domainLabelTail) >>= \case
        Just x -> do
          when (domainSize == 253) do
            fail "Domain is longer than then 253 chars, which is the maximum as per RFC-1035"
          when (labelSize == 63) do
            fail "Domain label is longer than 63 chars, which is the maximum as per RFC-1035"
          go (succ domainSize) (succ labelSize) (x : chars)
        Nothing ->
          pure (fromString (reverse chars), domainSize)
