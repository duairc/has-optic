{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
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

module Data.Optic.Instances.Monoid
    ()
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Tuple)


-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative)
#endif
import           Data.Functor.Identity (Identity (Identity))
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Monoid
                     ( All (All)
#if MIN_VERSION_base(4, 8, 0)
                     , Alt (Alt)
#endif
                     , Any (Any)
                     , Dual (Dual)
                     , Endo (Endo)
                     , First (First)
                     , Last (Last)
                     , Sum (Sum)
                     )
import qualified Data.Monoid as M (Product (Product))
import           Data.Semigroup
                     ( Max (Max)
                     , Min (Min)
                     , Option (Option)
                     , WrappedMonoid (WrapMonoid)
                     )
import qualified Data.Semigroup as S
                     ( First (First)
                     , Last (Last)
                     )
import           Prelude hiding (head, tail, init, last)


-- has-optic -----------------------------------------------------------------
import           Data.Optic.Accessors
                     ( _All
                     , _Alt
                     , _Any
                     , _Dual
                     , _Endo
                     , _First
                     , _Just
                     , _Last
                     , _Max
                     , _Min
                     , _Nothing
                     , _Option
                     , _Product
                     , _Sum
                     , _WrapMonoid
                     , head
                     , tail
                     )
import           Data.Optic.Instances.Sum ()


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Choice, Profunctor, dimap)


-- types ---------------------------------------------------------------------
import           Type.List (Cons, Nil)


-- types-th ------------------------------------------------------------------
import           Type.TH ()


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("All") p f All All Bool Bool where
    optic' _ = dimap (\(All a) -> a) (fmap All)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $(1) p f All All Bool Bool where
    optic' _ = _All
    {-# INLINE optic' #-}


#if MIN_VERSION_base(4, 8, 0)
------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Alt") p f (Alt f a) (Alt f a) (f a) (f a)
  where
    optic' _ = dimap (\(Alt a) -> a) (fmap Alt)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (Alt f a) (Alt f a) (f a) (f a)
  where
    optic' _ = _Alt
    {-# INLINE optic' #-}


#endif
------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("Any") p f Any Any Bool Bool where
    optic' _ = dimap (\(Any a) -> a) (fmap Any)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $(1) p f Any Any Bool Bool where
    optic' _ = _Any
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("Dual") p f (Dual a) (Dual b) a b
  where
    optic' _ = dimap (\(Dual a) -> a) (fmap Dual)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $(1) p f (Dual a) (Dual b) a b where
    optic' _ = _Dual
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Endo") p f (Endo a) (Endo b) (a -> a) (b -> b)
  where
    optic' _ = dimap (\(Endo a) -> a) (fmap Endo)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (Endo a) (Endo b) (a -> a) (b -> b)
  where
    optic' _ = _Endo
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("First") p f (First a) (First b) (Maybe a) (Maybe b)
  where
    optic' _ = dimap (\(First a) -> a) (fmap First)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (First a) (First b) (Maybe a) (Maybe b)
  where
    optic' _ = _First
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Nothing") p f (First a) (First a) () ()
  where
    optic' _ = _First . _Nothing
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Just") p f (First a) (First b) a b
  where
    optic' _ = _First . _Just
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Last") p f (Last a) (Last b) (Maybe a) (Maybe b)
  where
    optic' _ = dimap (\(Last a) -> a) (fmap Last)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (Last a) (Last b) (Maybe a) (Maybe b)
  where
    optic' _ = _Last
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Nothing") p f (Last a) (Last a) () ()
  where
    optic' _ = _Last . _Nothing
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Just") p f (Last a) (Last b) a b
  where
    optic' _ = _Last . _Just
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Product") p f (M.Product a) (M.Product b) a b
  where
    optic' _ = dimap (\(M.Product a) -> a) (fmap M.Product)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (M.Product a) (M.Product b) a b
  where
    optic' _ = _Product
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("Sum") p f (Sum a) (Sum b) a b
  where
    optic' _ = dimap (\(Sum a) -> a) (fmap Sum)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $(1) p f (Sum a) (Sum b) a b where
    optic' _ = _Sum
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("Max") p f (Max a) (Max b) a b
  where
    optic' _ = dimap (\(Max a) -> a) (fmap Max)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $(1) p f (Max a) (Max b) a b where
    optic' _ = _Max
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("Min") p f (Min a) (Min b) a b
  where
    optic' _ = dimap (\(Min a) -> a) (fmap Min)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $(1) p f (Min a) (Min b) a b where
    optic' _ = _Min
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Option") p f (Option a) (Option b) (Maybe a) (Maybe b)
  where
    optic' _ = dimap (\(Option a) -> a) (fmap Option)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (Option a) (Option b) (Maybe a) (Maybe b)
  where
    optic' _ = _Option
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Nothing") p f (Option a) (Option a) () ()
  where
    optic' _ = _Option . _Nothing
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Just") p f (Option a) (Option b) a b
  where
    optic' _ = _Option . _Just
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("WrapMonoid") p f (WrappedMonoid a) (WrappedMonoid b) a b
  where
    optic' _ = dimap (\(WrapMonoid a) -> a) (fmap WrapMonoid)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (WrappedMonoid a) (WrappedMonoid b) a b
  where
    optic' _ = _WrapMonoid
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("First") p f (S.First a) (S.First b) a b
  where
    optic' _ = dimap (\(S.First a) -> a) (fmap S.First)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (S.First a) (S.First b) a b
  where
    optic' _ = _First
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $("Last") p f (S.Last a) (S.Last b) a b
  where
    optic' _ = dimap (\(S.Last a) -> a) (fmap S.Last)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) =>
    Has $(1) p f (S.Last a) (S.Last b) a b
  where
    optic' _ = _Last
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance Functor f => Has $("head") (->) f (NonEmpty a) (NonEmpty a) a a where
    optic' _ f (a :| as) = fmap (:| as) (f a)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f (NonEmpty a) (NonEmpty a) a a where
    optic' _ = head
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance Functor f => Has $("tail") (->) f (NonEmpty a) (NonEmpty a) [a] [a]
  where
    optic' _ f (a :| as) = fmap (a :|) (f as)
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f (NonEmpty a) (NonEmpty a) [a] [a] where
    optic' _ = tail
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Profunctor p, Functor f) => Has $("Cons") p f
    (NonEmpty a)
    (NonEmpty b)
    (Tuple (Cons a (Cons [a] Nil)))
    (Tuple (Cons b (Cons [b] Nil)))
  where
    optic' _ = dimap toTuple (fmap fromTuple)
      where
        toTuple :: NonEmpty a -> Tuple (Cons a (Cons [a] Nil))
        toTuple (a :| as) = Cons (Identity a) (Cons (Identity as) Nil)
        {-# INLINE toTuple #-}
        fromTuple :: Tuple (Cons a (Cons [a] Nil)) -> NonEmpty a
        fromTuple (Cons (Identity a) (Cons (Identity as) Nil)) = a :| as
#if __GLASGOW_HASKELL__ < 800
        fromTuple _ = undefined
#endif
        {-# INLINE fromTuple #-}
    {-# INLINE optic' #-}
