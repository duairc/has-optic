{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
#else
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#endif
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Has.Lens
    ( Has (has)
    , Lens
    , Simple
    , Has'
    , Lens'
    , lens
    )
where

------------------------------------------------------------------------------
class Has n s t a b | n s -> a, n t -> b, n s a -> t, n t a -> s where
    has :: proxy n -> Lens s t a b


------------------------------------------------------------------------------
type Lens s t a b = Functor f => (a -> f b) -> s -> f t


------------------------------------------------------------------------------
type Simple f (s :: *) (a :: *) = f s s a a


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 704
type Has' n s a = Simple (Has n) s a
#else
class Has n s s a a => Has' n s a | n s -> a
instance Has n s s a a => Has' n s a
#endif


------------------------------------------------------------------------------
type Lens' s a = Simple Lens s a


------------------------------------------------------------------------------
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = fmap (sbt s) (afb (sa s))
{-# INLINE lens #-}
