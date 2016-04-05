{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Optic.Instances.Tuple
    ()
where

-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, optic')


-- types-th ------------------------------------------------------------------
import           Type.TH ()


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f (a, b) (a', b) a a' where
    optic' _ f (a, b) = fmap (\a' -> (a' , b)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f (a, b, c) (a', b, c) a a' where
    optic' _ f (a, b, c) = fmap (\a' -> (a', b, c)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f (a, b, c, d) (a', b, c, d) a a' where
    optic' _ f (a, b, c, d) = fmap (\a' -> (a', b, c, d)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f (a, b, c, d, e) (a', b, c, d, e) a a'
  where
    optic' _ f (a, b, c, d, e) = fmap (\a' -> (a', b, c, d, e)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f
    (a, b, c, d, e, f_)
    (a', b, c, d, e, f_)
    a
    a'
  where
    optic' _ f (a, b, c, d, e, f_) = fmap (\a' -> (a', b, c, d, e, f_)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f
    (a, b, c, d, e, f_, g)
    (a', b, c, d, e, f_, g)
    a
    a'
  where
    optic' _ f (a, b, c, d, e, f_, g) =
        fmap (\a' -> (a', b, c, d, e, f_, g)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f
    (a, b, c, d, e, f_, g, h)
    (a', b, c, d, e, f_, g, h)
    a
    a'
  where
    optic' _ f (a, b, c, d, e, f_, g, h) =
        fmap (\a' -> (a', b, c, d, e, f_, g, h)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has $(1) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a', b, c, d, e, f_, g, h, i)
    a
    a'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\a' -> (a', b, c, d, e, f_, g, h, i)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f (a, b) (a, b') b b' where
    optic' _ f (a, b) = fmap (\b' -> (a, b')) (f b)


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f (a, b, c) (a, b', c) b b' where
    optic' _ f (a, b, c) = fmap (\b' -> (a, b', c)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f (a, b, c, d) (a, b', c, d) b b' where
    optic' _ f (a, b, c, d) = fmap (\b' -> (a, b', c, d)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f
    (a, b, c, d, e)
    (a, b', c, d, e)
    b
    b'
  where
    optic' _ f (a, b, c, d, e) = fmap (\b' -> (a, b', c, d, e)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f
    (a, b, c, d, e, f_)
    (a, b', c, d, e, f_)
    b
    b'
  where
    optic' _ f (a, b, c, d, e, f_) = fmap (\b' -> (a, b', c, d, e, f_)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f
    (a, b, c, d, e, f_, g)
    (a, b', c, d, e, f_, g)
    b
    b'
  where
    optic' _ f (a, b, c, d, e, f_, g) =
        fmap (\b' -> (a, b', c, d, e, f_, g)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b', c, d, e, f_, g, h)
    b
    b'
  where
    optic' _ f (a, b, c, d, e, f_, g, h) =
        fmap (\b' -> (a, b', c, d, e, f_, g, h)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has $(2) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b', c, d, e, f_, g, h, i)
    b
    b'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\b' -> (a, b', c, d, e, f_, g, h, i)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has $(3) (->) f (a, b, c) (a, b, c') c c' where
    optic' _ f (a, b, c) = fmap (\c' -> (a, b, c')) (f c)


------------------------------------------------------------------------------
instance Functor f => Has $(3) (->) f (a, b, c, d) (a, b, c', d) c c' where
    optic' _ f (a, b, c, d) = fmap (\c' -> (a, b, c', d)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has $(3) (->) f
    (a, b, c, d, e)
    (a, b, c', d, e)
    c
    c'
  where
    optic' _ f (a, b, c, d, e) = fmap (\c' -> (a, b, c', d, e)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has $(3) (->) f
    (a, b, c, d, e, f_)
    (a, b, c', d, e, f_)
    c
    c'
  where
    optic' _ f (a, b, c, d, e, f_) = fmap (\c' -> (a, b, c', d, e, f_)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has $(3) (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c', d, e, f_, g)
    c
    c'
  where
    optic' _ f (a, b, c, d, e, f_, g)
        = fmap (\c' -> (a, b, c', d, e, f_, g)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has $(3) (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c', d, e, f_, g, h)
    c
    c'
  where
    optic' _ f (a, b, c, d, e, f_, g, h) =
        fmap (\c' -> (a, b, c', d, e, f_, g, h)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has $(3) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c', d, e, f_, g, h, i)
    c
    c'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\c' -> (a, b, c', d, e, f_, g, h, i)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has $(4) (->) f (a, b, c, d) (a, b, c, d') d d' where
    optic' _ f (a, b, c, d) = fmap (\d' -> (a, b, c, d')) (f d)


------------------------------------------------------------------------------
instance Functor f => Has $(4) (->) f
    (a, b, c, d, e)
    (a, b, c, d', e)
    d
    d'
  where
    optic' _ f (a, b, c, d, e) = fmap (\d' -> (a, b, c, d', e)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has $(4) (->) f
    (a, b, c, d, e, f_)
    (a, b, c, d', e, f_)
    d
    d'
  where
    optic' _ f (a, b, c, d, e, f_) = fmap (\d' -> (a, b, c, d', e, f_)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has $(4) (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c, d', e, f_, g)
    d
    d'
  where
    optic' _ f (a, b, c, d, e, f_, g) =
        fmap (\d' -> (a, b, c, d', e, f_, g)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has $(4) (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d', e, f_, g, h)
    d
    d'
  where
    optic' _ f (a, b, c, d, e, f_, g, h) =
        fmap (\d' -> (a, b, c, d', e, f_, g, h)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has $(4) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d', e, f_, g, h, i)
    d
    d'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\d' -> (a, b, c, d', e, f_, g, h, i)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has $(5) (->) f
    (a, b, c, d, e)
    (a, b, c, d, e')
    e
    e'
  where
    optic' _ f (a, b, c, d, e) = fmap (\e' -> (a, b, c, d, e')) (f e)


------------------------------------------------------------------------------
instance Functor f => Has $(5) (->) f
    (a, b, c, d, e, f_)
    (a, b, c, d, e', f_)
    e
    e'
  where
    optic' _ f (a, b, c, d, e, f_) = fmap (\e' -> (a, b, c, d, e', f_)) (f e)


------------------------------------------------------------------------------
instance Functor f => Has $(5) (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c, d, e', f_, g)
    e
    e'
  where
    optic' _ f (a, b, c, d, e, f_, g) =
        fmap (\e' -> (a, b, c, d, e', f_, g)) (f e)


------------------------------------------------------------------------------
instance Functor f => Has $(5) (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d, e', f_, g, h)
    e
    e'
  where
    optic' _ f (a, b, c, d, e, f_, g, h) =
        fmap (\e' -> (a, b, c, d, e', f_, g, h)) (f e)


------------------------------------------------------------------------------
instance Functor f => Has $(5) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e', f_, g, h, i)
    e
    e'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\e' -> (a, b, c, d, e', f_, g, h, i)) (f e)


------------------------------------------------------------------------------
instance Functor f => Has $(6) (->) f
    (a, b, c, d, e, f_)
    (a, b, c, d, e, f_')
    f_
    f_'
  where
    optic' _ f (a, b, c, d, e, f_) = fmap (\f_' -> (a, b, c, d, e, f_')) (f f_)


------------------------------------------------------------------------------
instance Functor f => Has $(6) (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c, d, e, f_', g)
    f_
    f_'
  where
    optic' _ f (a, b, c, d, e, f_, g) =
        fmap (\f_' -> (a, b, c, d, e, f_', g)) (f f_)


------------------------------------------------------------------------------
instance Functor f => Has $(6) (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d, e, f_', g, h)
    f_
    f_'
  where
    optic' _ f (a, b, c, d, e, f_, g, h) =
        fmap (\f_' -> (a, b, c, d, e, f_', g, h)) (f f_)


------------------------------------------------------------------------------
instance Functor f => Has $(6) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e, f_', g, h, i)
    f_
    f_'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\f_' -> (a, b, c, d, e, f_', g, h, i)) (f f_)


------------------------------------------------------------------------------
instance Functor f => Has $(7) (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c, d, e, f_, g')
    g
    g'
  where
    optic' _ f (a, b, c, d, e, f_, g) =
        fmap (\g' -> (a, b, c, d, e, f_, g')) (f g)


------------------------------------------------------------------------------
instance Functor f => Has $(7) (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d, e, f_, g', h)
    g
    g'
  where
    optic' _ f (a, b, c, d, e, f_, g, h) =
        fmap (\g' -> (a, b, c, d, e, f_, g', h)) (f g)


------------------------------------------------------------------------------
instance Functor f => Has $(7) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e, f_, g', h, i)
    g
    g'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\g' -> (a, b, c, d, e, f_, g', h, i)) (f g)


------------------------------------------------------------------------------
instance Functor f => Has $(8) (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d, e, f_, g, h')
    h
    h'
  where
    optic' _ f (a, b, c, d, e, f_, g, h) =
        fmap (\h' -> (a, b, c, d, e, f_, g, h')) (f h)


------------------------------------------------------------------------------
instance Functor f => Has $(8) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e, f_, g, h', i)
    h
    h'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i)
        = fmap (\h' -> (a, b, c, d, e, f_, g, h', i)) (f h)


------------------------------------------------------------------------------
instance Functor f => Has $(9) (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e, f_, g, h, i')
    i
    i'
  where
    optic' _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\i' -> (a, b, c, d, e, f_, g, h, i')) (f i)
