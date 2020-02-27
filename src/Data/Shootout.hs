{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, EmptyCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyDataDecls, TypeOperators, LiberalTypeSynonyms
             , ExistentialQuantification, ExplicitForAll, InstanceSigs
             , PartialTypeSignatures, NoStarIsType #-}
{-# LANGUAGE GADTSyntax, GADTs, FlexibleContexts, MultiParamTypeClasses
             , FlexibleInstances, TypeSynonymInstances, DefaultSignatures
             , PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies, ConstraintKinds, ScopedTypeVariables
             , ImplicitParams, TypeApplications, QuantifiedConstraints
             , RankNTypes, TypeFamilyDependencies, TypeFamilies
             , KindSignatures #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
-- {-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- |
-- Module      : Data.Shootout
-- Description : The interface to the modules of shootout: A small n-player game.
-- Copyright   : (c) shootout, 2019
-- License     : BSD-3-Clause
-- Maintainer  : greatBigDot@gmail.com
-- Stability   : experimental
--
-- TODO: Write description.
module Data.Shootout where

import Data.Kind
import Data.Nat
import Data.Type.Nat
import System.Random
-- import Orphanage
import Data.Fin(Fin())
import Data.Vec.Lazy(Vec(..), (!), universe)
import qualified Data.Vec.Lazy as V
import qualified Data.Fin as Fin

type ℕ = Nat

(⊥) ∷ a
(⊥) = undefined

-- Available moves in an n-player game.
-- data Move ℕ = Block | Reload | Shoot m
-- data State n = MkState { maxBullets ∷ ℕ, history ∷ [Vec n (Maybe (Move n))], Vec n (Maybe (Fin b))a}

-- What if I abandon type safety almost entirely?
data Move n = Block | Reload | Shoot (Fin n) deriving (Eq, Show)
data State n m = MkState { history ∷ [Vec n (Maybe (Move n))], bullets ∷ Vec n (Maybe (Fin m)) }
type Player n m = State n m → StdGen → Move n

blocker ∷ Player n m
blocker _ _ = Block

reloader ∷ Player n m
reloader _ _ = Reload

shooter ∷ Player ('S n) m
shooter _ _ = Shoot Fin.Z

step ∷ State n m → Vec n (Maybe (Move n)) → State n m
step state moves = MkState (moves : history state) (⊥)

idk ∷ Vec n (Maybe (Fin ('S m))) → Vec n (Maybe (Move n)) → Fin n → Maybe (Fin ('S m))
idk bullets moves fn
  | Nothing ← bullets ! fn = Nothing
  | any (== (Just (Fin.S _), Just (Shoot fn))) (V.zipWith (,) bullets moves) = (⊥)

-- newBullet ∷ State n m → Vec n (Maybe (Move n)) → Fin n → State n m

wasShot ∷ ∀(n ∷ ℕ)(m ∷ ℕ). SNatI n => State n m → Vec n (Maybe (Move n)) → Fin n → Maybe Bool
wasShot (MkState _ bullets) xs fn = case xs ! fn of
  Nothing    → Nothing
  Just Block → Just False
  _          → (⊥) (universe ∷ Vec n (Fin n))
    -- Just (any (\== Just (Shoot fn)) xs)


{-
step state [] = state
step state (Block : moves) = 
-
-- blocker ∷ Player m n
-- blocker _ = Block
-}


-- randomPlayer ∷ SNatI n => Player ('S n) m
-- randomPlayer _ g = fst (random g)


