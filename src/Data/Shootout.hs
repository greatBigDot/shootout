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
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
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
module Data.Shootout ( module Data.Shootout.Base ) where

import Data.Shootout.Base(foo)
