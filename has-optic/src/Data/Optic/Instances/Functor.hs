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

module Data.Optic.Instances.Functor
    ()
where

-- base ----------------------------------------------------------------------
import           Control.Applicative (Const (Const))
import           Data.Functor.Compose (Compose (Compose))
import           Data.Functor.Identity (Identity (Identity))


-- has-optic -----------------------------------------------------------------
import           Data.Optic.Accessors (_Compose, _Const, _Identity)


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap)


-- types-th ------------------------------------------------------------------
import           Type.TH ()


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Compose") p f (Compose g h a) (Compose k l b) (g (h a)) (k (l b))
  where
    optic' _ = dimap (\(Compose a) -> a) (fmap Compose)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (Compose g h a) (Compose k l b) (g (h a)) (k (l b))
  where
    optic' _ = _Compose
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("getCompose") p f
    (Compose g h a)
    (Compose k l b)
    (g (h a))
    (k (l b))
  where
    optic' _ = _Compose
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Const") p f (Const a c) (Const b c) a b
  where
    optic' _ = dimap (\(Const a) -> a) (fmap Const)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (Const a c) (Const b c) a b
  where
    optic' _ = _Const
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("getConst") p f (Const a c) (Const b c) a b
  where
    optic' _ = _Const
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Identity") p f (Identity a) (Identity b) a b
  where
    optic' _ = dimap (\(Identity a) -> a) (fmap Identity)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (Identity a) (Identity b) a b
  where
    optic' _ = _Identity
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("runIdentity") p f (Identity a) (Identity b) a b
  where
    optic' _ = _Identity
    {-# INLINE optic' #-}
