{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

module Data.Optic.Instances.Sum
    ()
where

-- anonymous-data ------------------------------------------------------------
import           Data.Anonymous.Product (Product (Cons, Nil), Tuple)


-- base ----------------------------------------------------------------------
#if !MIN_VERSION_base(4, 8, 0)
import           Control.Applicative (Applicative, pure)
#endif
import           Data.Functor.Identity (Identity (Identity))


-- has-optic -----------------------------------------------------------------
import           Data.Optic.Accessors
                     ( _Nothing
                     , _Just
                     , _Left
                     , _Right
                     , _Nil
                     , _Cons
                     )


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- profunctors ---------------------------------------------------------------
import           Data.Profunctor (Choice, dimap, right')


-- types ---------------------------------------------------------------------
import           Type.List (Cons, Nil)


-- types-th ------------------------------------------------------------------
import           Type.TH ()


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Nothing") p f (Maybe a) (Maybe a) () ()
  where
    optic' _ = dimap
        (\s -> maybe (Right ()) (const (Left s)) s)
        (either pure (fmap (const Nothing)))
            . right'
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) => Has $(1) p f (Maybe a) (Maybe a) () ()
  where
    optic' _ = _Nothing
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Just") p f (Maybe a) (Maybe b) a b
  where
    optic' _ = dimap
        (maybe (Left Nothing) Right)
        (either pure (fmap Just))
            . right'
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) => Has $(2) p f (Maybe a) (Maybe b) a b
  where
    optic' _ = _Just
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Left") p f (Either a b) (Either c b) a c
  where
    optic' _ = dimap
        (either Right (Left . Right))
        (either pure (fmap Left))
            . right'
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $(1) p f (Either a b) (Either c b) a c
  where
    optic' _ = _Left
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $("Right") p f (Either a b) (Either a c) b c
  where
    optic' _ = dimap
        (either (Left . Left) Right)
        (either pure (fmap Right))
            . right'
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) =>
    Has $(2) p f (Either a b) (Either a c) b c
  where
    optic' _ = _Right
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) => Has $("Nil") p f [a] [a] () () where
    optic' _ = dimap
        (\s -> foldr (const (const (Left s))) (Right ()) s)
        (either pure (fmap (const [])))
            . right'
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) => Has $(1) p f [a] [a] () () where
    optic' _ = _Nil
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) => Has $("Cons") p f
    [a]
    [b]
    (Tuple (Cons a (Cons [a] Nil)))
    (Tuple (Cons b (Cons [b] Nil)))
  where
    optic' _ = dimap
        (\s -> case s of
            a : as -> Right (toTuple a as)
            [] -> Left [])
        (either pure (fmap fromTuple))
            . right'
      where
        toTuple :: a -> [a] -> Tuple (Cons a (Cons [a] Nil))
        toTuple a as = Cons (Identity a) (Cons (Identity as) Nil)
        {-# INLINE toTuple #-}
        fromTuple :: Tuple (Cons a (Cons [a] Nil)) -> [a]
        fromTuple (Cons (Identity a) (Cons (Identity as) Nil)) = a : as
#if __GLASGOW_HASKELL__ < 800
        fromTuple _ = undefined
#endif
        {-# INLINE fromTuple #-}
    {-# INLINE optic' #-}


------------------------------------------------------------------------------
instance (Choice p, Applicative f) => Has $(2) p f
    [a]
    [b]
    (Tuple (Cons a (Cons [a] Nil)))
    (Tuple (Cons b (Cons [b] Nil)))
  where
    optic' _ = _Cons
    {-# INLINE optic' #-}
