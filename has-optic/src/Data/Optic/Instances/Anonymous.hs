{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifdef DataPolyKinds
#include "overlap.h"
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Optic.Instances.Anonymous
    ()
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Record, Tuple)
import           Data.Field (Field (Field))
import           Data.Uncurry (Uncurry (Uncurry))


-- anonymous-data-lens -------------------------------------------------------
import qualified Data.Anonymous.Product.Lens as P (Key, key', Index, index')


-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Functor ((<$>))
#endif
import           Data.Functor.Identity (Identity (Identity))


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- types ---------------------------------------------------------------------
import           Type.Meta (Proxy (Proxy))
import           GHC.TypeLits.Compat ((:-))


-- types-th ------------------------------------------------------------------
import           Type.TH ()


------------------------------------------------------------------------------
instance (Functor f, P.Index (n :- $(1)) as bs a b) =>
    Has n (->) f (Tuple as) (Tuple bs) a b
  where
    optic' _ f = P.index' proxy (\(Identity a) -> Identity <$> f a)
      where
        proxy = Proxy :: Proxy (n :- $(1))


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 800
instance (Functor f, P.Key n as bs a b) =>
    Has n (->) f (Record as) (Record bs) a b
#else
instance (Functor f, P.Key n as as a a) => Has n (->) f
    (Record as)
    (Record as)
    a
    a
#endif
  where
    optic' p f = P.key' p (\(Uncurry (Field a)) -> Uncurry . Field <$> f a)

#ifdef DataPolyKinds

------------------------------------------------------------------------------
instance __OVERLAPS__ (Functor f, P.Index n as bs '(s, a) '(s, b)) =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' p f = P.index' p (\(Uncurry (Field a)) -> Uncurry . Field <$> f a)

#endif
