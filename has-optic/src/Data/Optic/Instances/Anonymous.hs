{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
                     , LookupIndex'
                     , UpdateIndex'
                     , index'
                     , LookupKey'
                     , UpdateKey'
                     , key'
                     )
import qualified Data.Field as F (traverse)
#ifdef ClosedTypeFamilies
import           Type.List.Fields
                     ( LookupIndex
                     , LookupKey
                     , UpdateIndex
                     , UpdateKey
                     )
#endif


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
#ifdef ClosedTypeFamilies
    , b ~ LookupIndex (n :- One) bs
    , as ~ UpdateIndex (n :- One) a bs
#endif
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
#ifdef ClosedTypeFamilies
    , b ~ LookupKey n bs
    , as ~ UpdateKey n a bs
#endif
    )
  =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' p = key' p . F.traverse
#ifdef DataPolyKinds


------------------------------------------------------------------------------
instance
    ( Functor f
    , LookupIndex' (n :- One) as (Pair s a)
    , UpdateIndex' (n :- One) (Pair s b) as bs
#ifdef ClosedTypeFamilies
    , Pair s b ~ LookupIndex (n :- One) bs
    , as ~ UpdateIndex (n :- One) (Pair s a) bs
#endif
    )
  =>
    Has n (->) f (Record as) (Record bs) a b
  where
    optic' _ = index' (Proxy :: Proxy (n :- One)) . F.traverse
#endif
