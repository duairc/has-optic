{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Data.Field (Field (Field), Option (Option))
#ifdef ClosedTypeFamilies
import           Type.List.Fields
                     ( LookupIndex
                     , LookupKey
                     , UpdateIndex
                     , UpdateKey
                     )
#endif


-- has-optic -----------------------------------------------------------------
import           Data.Optic.Instances.Functors ()


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Profunctor, dimap)


-- types ---------------------------------------------------------------------
import           GHC.TypeLits.Compat (KnownSymbol, (:-), One)
import           Type.Meta (Proxy (Proxy))
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
    optic' _ = index' (Proxy :: Proxy (n :- One)) . optic' one


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupKey' n as a
    , UpdateKey' n b as bs
    , KnownSymbol n
#ifdef ClosedTypeFamilies
    , b ~ LookupKey n bs
    , as ~ UpdateKey n a bs
#endif
    )
  =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' p = key' p . optic' one
#ifdef DataPolyKinds


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupIndex' (n :- One) as (Pair s a)
    , UpdateIndex' (n :- One) (Pair s b) as bs
    , KnownSymbol s
#ifdef ClosedTypeFamilies
    , Pair s b ~ LookupIndex (n :- One) bs
    , as ~ UpdateIndex (n :- One) (Pair s a) bs
#endif
    )
  =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' _ = index' (Proxy :: Proxy (n :- One)) . optic' one
#endif


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupKey' n as a
    , UpdateKey' n b as bs
    , KnownSymbol n
#ifdef ClosedTypeFamilies
    , b ~ LookupKey n bs
    , as ~ UpdateKey n a bs
#endif
    )
  =>
    Has n (->) f (Options as) (Options bs) (Maybe a) (Maybe b)
  where
    optic' p = key' p . optic' one
#ifdef DataPolyKinds


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupIndex' (n :- One) as (Pair s a)
    , UpdateIndex' (n :- One) (Pair s b) as bs
    , KnownSymbol s
#ifdef ClosedTypeFamilies
    , Pair s b ~ LookupIndex (n :- One) bs
    , as ~ UpdateIndex (n :- One) (Pair s a) bs
#endif
    )
  =>
    Has n (->) f (Options as) (Options bs) (Maybe a) (Maybe b)
  where
    optic' _ = index' (Proxy :: Proxy (n :- One)) . optic' one
#endif


------------------------------------------------------------------------------
instance (Profunctor p, Functor f, KnownSymbol s) =>
    Has One p f (Field (Pair s a)) (Field (Pair s b)) a b
  where
    optic' _ = dimap (\(Field a) -> a) (fmap Field)
    {-# INLINE optic' #-}


#ifdef DataPolyKinds
------------------------------------------------------------------------------
instance (Profunctor p, Functor f, KnownSymbol s) => Has s p f
    (Field (Pair s a))
    (Field (Pair s b))
    a
    b
  where
    optic' _ = dimap (\(Field a) -> a) (fmap Field)
    {-# INLINE optic' #-}


#endif
------------------------------------------------------------------------------
instance (Profunctor p, Functor f, KnownSymbol s) =>
    Has One p f (Option (Pair s a)) (Option (Pair s b)) (Maybe a) (Maybe b)
  where
    optic' _ = dimap (\(Option a) -> a) (fmap Option)
    {-# INLINE optic' #-}


#ifdef DataPolyKinds
------------------------------------------------------------------------------
instance (Profunctor p, Functor f, KnownSymbol s) => Has s p f
    (Option (Pair s a))
    (Option (Pair s b))
    (Maybe a)
    (Maybe b)
  where
    optic' _ = dimap (\(Option a) -> a) (fmap Option)
    {-# INLINE optic' #-}


#endif
------------------------------------------------------------------------------
one :: Proxy One
one = Proxy
