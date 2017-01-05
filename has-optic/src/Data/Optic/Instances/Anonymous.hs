{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
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

#include "overlap.h"

module Data.Optic.Instances.Anonymous
    ()
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product
                     ( Record
                     , Tuple
                     , Options
                     , LookupIndex'
                     , UpdateIndex'
                     , index'
                     , LookupKey'
                     , UpdateKey'
                     , key'
                     )
import           Data.Labeled (Labeled (Labeled), Labeled1 (Labeled1))
#ifdef ClosedTypeFamilies
import           Type.List.Fields
                     ( LookupIndex
                     , LookupKey
                     , UpdateIndex
                     , UpdateKey
                     )
#endif


-- has-optic -----------------------------------------------------------------
import           Data.Optic.Accessors (_1, _First, _Identity, _Labeled1)
import           Data.Optic.Instances.Functor ()
import           Data.Optic.Instances.Monoid ()


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap)


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat ((:-), One)
import           Type.Meta (Known, Proxy (Proxy))
import           Type.Tuple.Pair (Pair)


-- types-th ------------------------------------------------------------------
import           Type.TH ()


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupIndex' (n :- One) as a
    , UpdateIndex' (n :- One) b as bs
#ifdef ClosedTypeFamilies
    , b ~ LookupIndex (n :- One) bs
    , as ~ UpdateIndex (n :- One) a bs
#endif
    )
  =>
    Has n (->) f (Tuple as) (Tuple bs) a b
  where
    optic' _ = index' (Proxy :: Proxy (n :- One)) . _1


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupKey' n as a
    , UpdateKey' n b as bs
    , Known n
#ifdef ClosedTypeFamilies
    , b ~ LookupKey n bs
    , as ~ UpdateKey n a bs
#endif
    )
  =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' p = key' p . _1 . _Identity
#ifdef DataPolyKinds


------------------------------------------------------------------------------
instance __OVERLAPS__
    ( Functor f
    , LookupIndex' (n :- One) as (Pair s a)
    , UpdateIndex' (n :- One) (Pair s b) as bs
    , Known s
#ifdef ClosedTypeFamilies
    , Pair s b ~ LookupIndex (n :- One) bs
    , as ~ UpdateIndex (n :- One) (Pair s a) bs
#endif
    )
  =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' _ = index' (Proxy :: Proxy (n :- One)) . _1 . _Identity
#endif


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupKey' n as a
    , UpdateKey' n b as bs
    , Known n
#ifdef ClosedTypeFamilies
    , b ~ LookupKey n bs
    , as ~ UpdateKey n a bs
#endif
    )
  =>
    Has n (->) f (Options as) (Options bs) (Maybe a) (Maybe b)
  where
    optic' p = key' p . _1 . _First
#ifdef DataPolyKinds


------------------------------------------------------------------------------
instance __OVERLAPS__
    ( Functor f
    , LookupIndex' (n :- One) as (Pair s a)
    , UpdateIndex' (n :- One) (Pair s b) as bs
    , Known s
#ifdef ClosedTypeFamilies
    , Pair s b ~ LookupIndex (n :- One) bs
    , as ~ UpdateIndex (n :- One) (Pair s a) bs
#endif
    )
  =>
    Has n (->) f (Options as) (Options bs) (Maybe a) (Maybe b)
  where
    optic' _ = index' (Proxy :: Proxy (n :- One)) . _1 . _First
#endif


------------------------------------------------------------------------------
instance (Profunctor p, Functor f, Known s) =>
    Has One p f (Labeled g (Pair s a)) (Labeled h (Pair s b)) (g a) (h b)
  where
    optic' _ = dimap (\(Labeled a) -> a) (fmap Labeled)
    {-# INLINE optic' #-}


#ifdef DataPolyKinds
------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (Profunctor p, Functor f, Known s) =>
    Has s p f (Labeled g (Pair s a)) (Labeled h (Pair s b)) (g a) (h b)
  where
    optic' _ = _1
    {-# INLINE optic' #-}


#endif
------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("Labeled1") p f
    (Labeled1 g s a)
    (Labeled1 h s b)
    (Labeled g (Pair s a))
    (Labeled h (Pair s b))
  where
    optic' _ = dimap (\(Labeled1 a) -> a) (fmap Labeled1)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has One p f
    (Labeled1 g s a)
    (Labeled1 h s b)
    (Labeled g (Pair s a))
    (Labeled h (Pair s b))
  where
    optic' _ = _Labeled1
    {-# INLINE optic' #-}
