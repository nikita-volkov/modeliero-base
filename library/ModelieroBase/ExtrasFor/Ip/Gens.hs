module ModelieroBase.ExtrasFor.Ip.Gens where

import ModelieroBase.Prelude
import Net.IPv4 qualified as IpV4
import Net.IPv6 qualified as IpV6
import Test.QuickCheck.Gen

ipV4 :: Gen IpV4.IPv4
ipV4 =
  IpV4.fromOctets
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

ipV6 :: Gen IpV6.IPv6
ipV6 =
  IpV6.fromWord32s
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
