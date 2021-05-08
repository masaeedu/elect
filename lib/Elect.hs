{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Elect where

import Data.Bifunctor (bimap)
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Semiring (Semiring (..))
import Data.These (These (..), these)
import Data.Void (Void)
import GHC.Generics (Generic)

-- https://gist.github.com/masaeedu/f6b77eb3ed7d8a62693ffd44f2d93181

-- | Let's suppose we have an election, and a voter can cast a vote of type 'a', or a veto of type 'e'.
data Elect a e
  = Abstain
  | Veto e
  | Vote a
  deriving (Eq, Show, Generic)

deriveBifunctor ''Elect
deriveBifoldable ''Elect
deriveBitraversable ''Elect

-- | Find an outcome that both voters would accept
consensus :: Elect a e -> Elect b f -> Elect (These a b) (These e f)
Abstain `consensus` x       = bimap That That x
x       `consensus` Abstain = bimap This This x

-- NB: By definition, everyone must be prepared to accept a veto
Veto e  `consensus` Veto f  = Veto $ These e f
Veto e  `consensus` _       = Veto $ This e
_       `consensus` Veto f  = Veto $ That f

Vote a  `consensus` Vote b  = Vote $ These a b

-- | The identity of 'consensus'
abstain :: Elect Void Void
abstain = Abstain

-- | Find a voter that would accept both outcomes
compromise :: Elect a e -> Elect b f -> Elect (These a b) (These e f)
Abstain `compromise` _       = Abstain
_       `compromise` Abstain = Abstain

-- NB: Everyone already accepts a veto, so that has no effect on a compromise (unless both people veto)
Veto e  `compromise` Veto f  = Veto $ These e f
Veto _  `compromise` x       = bimap That That x
x       `compromise` Veto _  = bimap This This x

Vote a  `compromise` Vote b  = Vote $ These a b

-- | The identity of `compromise`
veto :: e -> Elect Void e
veto = Veto

-----------------------------------------------------------------------------------------------

instance (Ord a, Semiring e) => Semiring (Elect a e)
  where
  a `plus` b = bimap (these id id max) (these id id plus) $ consensus a b
  zero = Abstain

  a `times` b = bimap (these id id min) (these id id times) $ compromise a b
  one = Veto one

-----------------------------------------------------------------------------------------------

-- | In a representative democracy, we can vote for a voter.
congress :: Elect (Elect a f) e -> Elect a (Either e f)
congress = \case
  Abstain -> Abstain
  Veto e -> Veto $ Left e
  Vote Abstain -> Abstain
  Vote (Veto f) -> Veto $ Right f
  Vote (Vote a) -> Vote a

-- | The identity of congress
vote :: a -> Elect a Void
vote = Vote
