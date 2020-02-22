{-# LANGUAGE UnicodeSyntax, TupleSections, LambdaCase, EmptyCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls, TypeOperators, LiberalTypeSynonyms
             , ExistentialQuantification, ExplicitForAll, InstanceSigs
             , PartialTypeSignatures, NoStarIsType #-}
{-# LANGUAGE GADTSyntax, GADTs, FlexibleContexts, MultiParamTypeClasses
             , FlexibleInstances , TypeSynonymInstances, DefaultSignatures
             , PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies, ConstraintKinds, ScopedTypeVariables
             , ImplicitParams, TypeApplications, QuantifiedConstraints
             , RankNTypes, TypeFamilyDependencies, TypeFamilies
             , KindSignatures #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}
module Main ( main ) where

import Data.Shootout(foo)

main :: IO ()
main = putStrLn $ "Hello, World! " ++ show foo
