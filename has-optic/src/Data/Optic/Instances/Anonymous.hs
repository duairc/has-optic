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
import           Data.Anonymous.Product
                     ( Record
                     , Tuple
                     , LookupIndex'
                     , UpdateIndex'
                     , index'
                     , LookupKey'
                     , UpdateKey'
                     , key'
                     )
import qualified Data.Field as F (traverse)


-- base ----------------------------------------------------------------------
import           Data.Functor.Identity (Identity (Identity))


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat ((:-), One)
import           Type.Meta (Proxy (Proxy))
import           Type.Tuple.Pair (Pair)


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupIndex' (n :- One) as a
    , UpdateIndex' (n :- One) b as bs
    )
  =>
    Has n (->) f (Tuple as) (Tuple bs) a b
  where
    optic' _ f = index' proxy (\(Identity a) -> fmap Identity (f a))
      where
        proxy = Proxy :: Proxy (n :- One)


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupKey' n as a
    , UpdateKey' n b as bs
    )
  =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' p = key' p . F.traverse
#ifdef DataPolyKinds


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupIndex' n as (Pair s a)
    , UpdateIndex' n (Pair s b) as bs
    )
  =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' p = index' p . F.traverse
#endif
{-
#ifdef DataPolyKinds

------------------------------------------------------------------------------
instance __OVERLAPS__ (Functor f, P.Index n as bs '(s, a) '(s, b)) =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' p = P.index' p . F.traverse

#endif
-}
