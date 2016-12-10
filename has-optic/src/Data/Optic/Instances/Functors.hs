{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Optic.Instances.Functors
    ()
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Data.Functor.Compose (Compose (Compose))
import           Data.Functor.Identity (Identity (Identity))


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap)


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat (One)


-- types-th ------------------------------------------------------------------
import           Type.TH ()


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has One p f (Compose g h a) (Compose k l b) (g (h a)) (k (l b))
  where
    optic' _ = dimap (\(Compose a) -> a) (fmap Compose)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Compose") p f (Compose g h a) (Compose k l b) (g (h a)) (k (l b))
  where
    optic' _ = dimap (\(Compose a) -> a) (fmap Compose)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has One p f (Const a c) (Const b c) a b
  where
    optic' _ = dimap (\(Const a) -> a) (fmap Const)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Const") p f (Const a c) (Const b c) a b
  where
    optic' _ = dimap (\(Const a) -> a) (fmap Const)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has One p f (Identity a) (Identity b) a b
  where
    optic' _ = dimap (\(Identity a) -> a) (fmap Identity)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Identity") p f (Identity a) (Identity b) a b
  where
    optic' _ = dimap (\(Identity a) -> a) (fmap Identity)
    {-# INLINE optic' #-}
