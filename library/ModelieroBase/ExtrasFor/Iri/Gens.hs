module ModelieroBase.ExtrasFor.Iri.Gens where

import Data.Text qualified as Text
import Iri.Data
import ModelieroBase.ExtrasFor.Ip.Gens qualified as IpGens
import ModelieroBase.ExtrasFor.QuickCheck.FreqGen qualified as FreqGen
import ModelieroBase.Prelude
import Test.QuickCheck.Gen

host :: Gen Host
host =
  oneof
    [ NamedHost <$> regName,
      IpV4Host <$> IpGens.ipV4,
      IpV6Host <$> IpGens.ipV6
    ]

regName :: Gen RegName
regName = go 253 []
  where
    go charsRemaining collectedLabels = do
      newLabel <- domainLabel
      let newLabelSize = case newLabel of
            DomainLabel text -> Text.length text
          newCharsRemaining = charsRemaining - newLabelSize
      if newCharsRemaining < 0
        then pure (RegName (fromList collectedLabels))
        else do
          shouldGoAgain <- arbitrary
          if shouldGoAgain && newCharsRemaining > 0
            then go newCharsRemaining (newLabel : collectedLabels)
            else pure (RegName (fromList (newLabel : collectedLabels)))

domainLabel :: Gen DomainLabel
domainLabel = do
  head <- genHeadChar
  tailSize <- choose (0, 63)
  tailList <- vectorOf tailSize genTailChar
  pure (DomainLabel (fromString (head : tailList)))
  where
    genHeadChar =
      [ FreqGen.charRange 'a' 'z',
        FreqGen.charRange 'A' 'Z',
        FreqGen.charRange 'а' 'я',
        FreqGen.charRange 'А' 'Я',
        FreqGen.charRange '0' '9'
      ]
        & mconcat
        & FreqGen.compile

    genTailChar =
      [ FreqGen.charRange 'a' 'z',
        FreqGen.charRange 'A' 'Z',
        FreqGen.charRange 'а' 'я',
        FreqGen.charRange 'А' 'Я',
        FreqGen.charRange '0' '9',
        FreqGen.set "-"
      ]
        & mconcat
        & FreqGen.compile
