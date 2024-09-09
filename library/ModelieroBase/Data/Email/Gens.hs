module ModelieroBase.Data.Email.Gens where

import ModelieroBase.ExtrasFor.QuickCheck.FreqGen qualified as FreqGen
import ModelieroBase.Prelude
import Test.QuickCheck.Gen

local :: Gen (NonEmpty Text)
local = do
  (head, sizeAvail) <- genLocalElement 64
  tail <- genTail (pred sizeAvail) []
  pure (head :| tail)
  where
    genTail !sizeAvail !list =
      if sizeAvail > 0
        then do
          (localElement, sizeAvail) <- genLocalElement sizeAvail
          shouldGo <- arbitrary
          if shouldGo
            then genTail (pred sizeAvail) (localElement : list)
            else pure (localElement : list)
        else pure (reverse list)

    genLocalElement sizeAvail = do
      head <- genHeadChar
      tailSize <- choose (0, sizeAvail)
      tailList <- vectorOf tailSize genTailChar
      pure (fromString (head : tailList), sizeAvail - 1 - tailSize)
      where
        genHeadChar =
          [ choose ('a', 'z'),
            choose ('A', 'Z')
          ]
            & oneof

        genTailChar =
          [ FreqGen.charRange 'a' 'z',
            FreqGen.charRange 'A' 'Z',
            FreqGen.charRange '0' '9',
            FreqGen.set "!#$%&'*+/=?^_`{|}~-"
          ]
            & mconcat
            & FreqGen.compile

domain :: Gen (NonEmpty Text)
domain = do
  (head, sizeAvail) <- genItem 253
  tail <- genTail (pred sizeAvail) []
  pure (head :| tail)
  where
    genTail !sizeAvail !list =
      if sizeAvail > 0
        then do
          (item, sizeAvail) <- genItem sizeAvail
          shouldGo <- arbitrary
          if shouldGo
            then genTail (pred sizeAvail) (item : list)
            else pure (item : list)
        else pure (reverse list)

    genItem sizeAvail = do
      head <- genHeadChar
      tailSize <- choose (0, min sizeAvail 63)
      tailList <- vectorOf tailSize genTailChar
      pure (fromString (head : tailList), sizeAvail - 1 - tailSize)
      where
        genHeadChar =
          [ FreqGen.charRange 'a' 'z',
            FreqGen.charRange 'A' 'Z',
            FreqGen.charRange '0' '9'
          ]
            & mconcat
            & FreqGen.compile

        genTailChar =
          [ FreqGen.charRange 'a' 'z',
            FreqGen.charRange 'A' 'Z',
            FreqGen.charRange '0' '9',
            FreqGen.set "-"
          ]
            & mconcat
            & FreqGen.compile
