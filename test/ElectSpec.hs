{-# OPTIONS_GHC -Wno-orphans #-}

module ElectSpec where

import Data.Proxy (Proxy (..))
import Elect (Elect)
import Generic.Random (genericArbitrary, uniform)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Classes (lawsCheckMany, semiringLaws)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Elect e a)
  where
  arbitrary = genericArbitrary uniform

unit_ElectionIsSemiring :: IO ()
unit_ElectionIsSemiring =
  lawsCheckMany
    [ ("Elect Int Int", [semiringLaws $ Proxy @(Elect Int Int)])
    ]
