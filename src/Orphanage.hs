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
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Orphanage
-- Description :
-- Copyright   : (c) greatBigDot, 2020
-- License     : BSD-3-Clause
-- Maintainer  : greatBigDot@gmail.com
-- Stability   : experimental
module Orphanage () where

import Data.Shootout
import System.Random
import Data.Nat
import Data.Type.Nat
import Data.Fin(Fin())
import qualified Data.Fin as Fin
import Control.Arrow

instance SNatI n => Random (Fin ('S n)) where
  random g = (fromInteger *** id) (randomR (0, m) g)
    where m = toInteger (fromInteger (-1) ∷ Fin ('S n))

  randomR (a,b) g = (fromInteger *** id) (randomR (a', b') g)
    where a' = toInteger a
          b' = toInteger b

instance SNatI n => Random (Move ('S n)) where
  -- Chooses each category 1/3 of the time, and if it chooses Shoot, chooses each
  -- of the n options 1/3 of the time.
  random g
    | cat == 1 = (Block,g2)
    | cat == 2 = (Reload,g2)
    | cat == 3 = (Shoot *** id) (random g2)
    | otherwise = error "impossible"
    where
      cat ∷ Integer
      (cat, g2) = randomR (1,3) g

  -- TODO: What's a reasonable way to implement this?
  randomR = error "unimplemented"

