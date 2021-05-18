{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Elect where

import Data.Bifunctor (bimap, first)
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Semiring (Semiring (..))
import Data.These (These (..), these)
import Data.Void (Void)
import GHC.Generics (Generic)
import Control.Monad (ap)

-- https://gist.github.com/masaeedu/f6b77eb3ed7d8a62693ffd44f2d93181

-- | Let's suppose we have an election, and a voter can cast a vote of type 'a', or a veto of type 'e'.
data Elect e a
  = Abstain
  | Veto e
  | Vote a
  deriving (Eq, Show, Functor, Generic)

deriveBifunctor ''Elect
deriveBifoldable ''Elect
deriveBitraversable ''Elect

-- | Find an outcome that both voters would accept
consensus :: Elect e a -> Elect f b -> Elect (These e f) (These a b)
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
compromise :: Elect e a -> Elect f b -> Elect (These e f) (These a b)
Abstain `compromise` _       = Abstain
_       `compromise` Abstain = Abstain

-- NB: Everyone already accepts a veto, so that has no effect on a compromise (unless both people veto)
Veto e  `compromise` Veto f  = Veto $ These e f
Veto _  `compromise` x       = bimap That That x
x       `compromise` Veto _  = bimap This This x

Vote a  `compromise` Vote b  = Vote $ These a b

-- | The identity of `compromise`
veto :: e -> Elect e Void
veto = Veto

-----------------------------------------------------------------------------------------------

instance (Semiring e, Ord a) => Semiring (Elect e a)
  where
  a `plus` b = bimap (these id id plus) (these id id max) $ consensus a b
  zero = Abstain

  a `times` b = bimap (these id id times) (these id id min) $ compromise a b
  one = Veto one

-----------------------------------------------------------------------------------------------

-- | In a representative democracy, we can vote for a voter.
congress :: Elect e (Elect f a) -> Elect (Either e f) a
congress = \case
  Abstain -> Abstain
  Veto e -> Veto $ Left e
  Vote Abstain -> Abstain
  Vote (Veto f) -> Veto $ Right f
  Vote (Vote a) -> Vote a

-- | The identity of congress
vote :: a -> Elect Void a
vote = Vote

instance Applicative (Elect e)
  where
  pure = return
  (<*>) = ap

instance Monad (Elect e)
  where
  return = Vote
  ma >>= amb = first (either id id) $ congress $ fmap amb ma
