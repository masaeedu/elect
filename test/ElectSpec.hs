{-# OPTIONS_GHC -Wno-orphans #-}
module ElectSpec where

import Test.QuickCheck.Classes (lawsCheckMany, semiringLaws)
import Elect
import Data.Proxy
import Test.QuickCheck (Arbitrary(..))
import Generic.Random

instance (Arbitrary e, Arbitrary a) => Arbitrary (Elect e a)
  where
  arbitrary = genericArbitrary uniform

unit_ElectionIsSemiring :: IO ()
unit_ElectionIsSemiring = lawsCheckMany
  [ ("Elect Int Int", [semiringLaws $ Proxy @(Elect Int Int)])
  ]
