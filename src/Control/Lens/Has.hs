{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
#endif

module Control.Lens.Has
    ( Has (has)
    , _1
    , _2
    , _3
    , _4
    , _5
    , _6
    , _7
    , _8
    , _9
    )
where

#if __GLASGOW_HASKELL__ >= 706
-- base ----------------------------------------------------------------------
import           GHC.TypeLits (Symbol)
#else
-- symbols -------------------------------------------------------------------
import           Type.Symbol.Internal ((:::), Nil, C, O)
import qualified Type.Symbol.Internal as T (I)
#define          Symbol *
#endif


------------------------------------------------------------------------------
data Proxy (k :: Symbol) = Proxy


------------------------------------------------------------------------------
type I =
#define X (T.I)
#if __GLASGOW_HASKELL__ >= 706
    "_1"
#else
    C X X X X X O X O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C X O O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif


------------------------------------------------------------------------------
type II =
#if __GLASGOW_HASKELL__ >= 706
    "_2"
#else
    C X X X X X O X O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C O X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif


------------------------------------------------------------------------------
type III =
#if __GLASGOW_HASKELL__ >= 706
    "_3"
#else
    C X X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C O X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif


------------------------------------------------------------------------------
type IV =
#if __GLASGOW_HASKELL__ >= 706
    "_4"
#else
    C O O X O X X O O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C O X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif


------------------------------------------------------------------------------
type V =
#if __GLASGOW_HASKELL__ >= 706
    "_5"
#else
    C X O X O X X O O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C O X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif


------------------------------------------------------------------------------
type VI =
#if __GLASGOW_HASKELL__ >= 706
    "_6"
#else
    C O X X O X X X O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C O X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif


------------------------------------------------------------------------------
type VII =
#if __GLASGOW_HASKELL__ >= 706
    "_7"
#else
    C X X X O X X O O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C O X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif


------------------------------------------------------------------------------
type VIII =
#if __GLASGOW_HASKELL__ >= 706
    "_8"
#else
    C O O O X X X O O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C O X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif


------------------------------------------------------------------------------
type IX =
#if __GLASGOW_HASKELL__ >= 706
    "_9"
#else
    C X O O X X X O O O O O O O O O O O O O O O O O O O O O O O O O O :::
    C O X O O X X O O O O O O O O O O O O O O O O O O O O O O O O O O ::: Nil
#endif
#undef X


------------------------------------------------------------------------------
class Has (n :: Symbol) p f s t a b
    | n s -> a
    , n t -> b
    , n s b -> t
    , n t a -> s
  where
    has :: proxy n -> p a (f b) -> p s (f t)


------------------------------------------------------------------------------
instance Functor f => Has I (->) f (a, b) (a', b) a a' where
    has _ f (a, b) = fmap (\a' -> (a' , b)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has I (->) f (a, b, c) (a', b, c) a a' where
    has _ f (a, b, c) = fmap (\a' -> (a', b, c)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has I (->) f (a, b, c, d) (a', b, c, d) a a' where
    has _ f (a, b, c, d) = fmap (\a' -> (a', b, c, d)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has I (->) f (a, b, c, d, e) (a', b, c, d, e) a a'
  where
    has _ f (a, b, c, d, e) = fmap (\a' -> (a', b, c, d, e)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has I (->) f
    (a, b, c, d, e, f_)
    (a', b, c, d, e, f_)
    a
    a'
  where
    has _ f (a, b, c, d, e, f_) = fmap (\a' -> (a', b, c, d, e, f_)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has I (->) f
    (a, b, c, d, e, f_, g)
    (a', b, c, d, e, f_, g)
    a
    a'
  where
    has _ f (a, b, c, d, e, f_, g) =
        fmap (\a' -> (a', b, c, d, e, f_, g)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has I (->) f
    (a, b, c, d, e, f_, g, h)
    (a', b, c, d, e, f_, g, h)
    a
    a'
  where
    has _ f (a, b, c, d, e, f_, g, h) =
        fmap (\a' -> (a', b, c, d, e, f_, g, h)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has I (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a', b, c, d, e, f_, g, h, i)
    a
    a'
  where
    has _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\a' -> (a', b, c, d, e, f_, g, h, i)) (f a)


------------------------------------------------------------------------------
instance Functor f => Has II (->) f (a, b) (a, b') b b' where
    has _ f (a, b) = fmap (\b' -> (a, b')) (f b)


------------------------------------------------------------------------------
instance Functor f => Has II (->) f (a, b, c) (a, b', c) b b' where
    has _ f (a, b, c) = fmap (\b' -> (a, b', c)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has II (->) f (a, b, c, d) (a, b', c, d) b b' where
    has _ f (a, b, c, d) = fmap (\b' -> (a, b', c, d)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has II (->) f
    (a, b, c, d, e)
    (a, b', c, d, e)
    b
    b'
  where
    has _ f (a, b, c, d, e) = fmap (\b' -> (a, b', c, d, e)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has II (->) f
    (a, b, c, d, e, f_)
    (a, b', c, d, e, f_)
    b
    b'
  where
    has _ f (a, b, c, d, e, f_) = fmap (\b' -> (a, b', c, d, e, f_)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has II (->) f
    (a, b, c, d, e, f_, g)
    (a, b', c, d, e, f_, g)
    b
    b'
  where
    has _ f (a, b, c, d, e, f_, g) =
        fmap (\b' -> (a, b', c, d, e, f_, g)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has II (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b', c, d, e, f_, g, h)
    b
    b'
  where
    has _ f (a, b, c, d, e, f_, g, h) =
        fmap (\b' -> (a, b', c, d, e, f_, g, h)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has II (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b', c, d, e, f_, g, h, i)
    b
    b'
  where
    has _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\b' -> (a, b', c, d, e, f_, g, h, i)) (f b)


------------------------------------------------------------------------------
instance Functor f => Has III (->) f (a, b, c) (a, b, c') c c' where
    has _ f (a, b, c) = fmap (\c' -> (a, b, c')) (f c)


------------------------------------------------------------------------------
instance Functor f => Has III (->) f (a, b, c, d) (a, b, c', d) c c' where
    has _ f (a, b, c, d) = fmap (\c' -> (a, b, c', d)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has III (->) f
    (a, b, c, d, e)
    (a, b, c', d, e)
    c
    c'
  where
    has _ f (a, b, c, d, e) = fmap (\c' -> (a, b, c', d, e)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has III (->) f
    (a, b, c, d, e, f_)
    (a, b, c', d, e, f_)
    c
    c'
  where
    has _ f (a, b, c, d, e, f_) = fmap (\c' -> (a, b, c', d, e, f_)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has III (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c', d, e, f_, g)
    c
    c'
  where
    has _ f (a, b, c, d, e, f_, g)
        = fmap (\c' -> (a, b, c', d, e, f_, g)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has III (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c', d, e, f_, g, h)
    c
    c'
  where
    has _ f (a, b, c, d, e, f_, g, h) =
        fmap (\c' -> (a, b, c', d, e, f_, g, h)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has III (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c', d, e, f_, g, h, i)
    c
    c'
  where
    has _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\c' -> (a, b, c', d, e, f_, g, h, i)) (f c)


------------------------------------------------------------------------------
instance Functor f => Has IV (->) f (a, b, c, d) (a, b, c, d') d d' where
    has _ f (a, b, c, d) = fmap (\d' -> (a, b, c, d')) (f d)


------------------------------------------------------------------------------
instance Functor f => Has IV (->) f
    (a, b, c, d, e)
    (a, b, c, d', e)
    d
    d'
  where
    has _ f (a, b, c, d, e) = fmap (\d' -> (a, b, c, d', e)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has IV (->) f
    (a, b, c, d, e, f_)
    (a, b, c, d', e, f_)
    d
    d'
  where
    has _ f (a, b, c, d, e, f_) = fmap (\d' -> (a, b, c, d', e, f_)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has IV (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c, d', e, f_, g)
    d
    d'
  where
    has _ f (a, b, c, d, e, f_, g) =
        fmap (\d' -> (a, b, c, d', e, f_, g)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has IV (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d', e, f_, g, h)
    d
    d'
  where
    has _ f (a, b, c, d, e, f_, g, h) =
        fmap (\d' -> (a, b, c, d', e, f_, g, h)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has IV (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d', e, f_, g, h, i)
    d
    d'
  where
    has _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\d' -> (a, b, c, d', e, f_, g, h, i)) (f d)


------------------------------------------------------------------------------
instance Functor f => Has V (->) f
    (a, b, c, d, e)
    (a, b, c, d, e')
    e
    e'
  where
    has _ f (a, b, c, d, e) = fmap (\e' -> (a, b, c, d, e')) (f e)


------------------------------------------------------------------------------
instance Functor f => Has V (->) f
    (a, b, c, d, e, f_)
    (a, b, c, d, e', f_)
    e
    e'
  where
    has _ f (a, b, c, d, e, f_) = fmap (\e' -> (a, b, c, d, e', f_)) (f e)


------------------------------------------------------------------------------
instance Functor f => Has V (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c, d, e', f_, g)
    e
    e'
  where
    has _ f (a, b, c, d, e, f_, g) =
        fmap (\e' -> (a, b, c, d, e', f_, g)) (f e)


------------------------------------------------------------------------------
instance Functor f => Has V (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d, e', f_, g, h)
    e
    e'
  where
    has _ f (a, b, c, d, e, f_, g, h) =
        fmap (\e' -> (a, b, c, d, e', f_, g, h)) (f e)


------------------------------------------------------------------------------
instance Functor f => Has V (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e', f_, g, h, i)
    e
    e'
  where
    has _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\e' -> (a, b, c, d, e', f_, g, h, i)) (f e)


------------------------------------------------------------------------------
instance Functor f => Has VI (->) f
    (a, b, c, d, e, f_)
    (a, b, c, d, e, f_')
    f_
    f_'
  where
    has _ f (a, b, c, d, e, f_) = fmap (\f_' -> (a, b, c, d, e, f_')) (f f_)


------------------------------------------------------------------------------
instance Functor f => Has VI (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c, d, e, f_', g)
    f_
    f_'
  where
    has _ f (a, b, c, d, e, f_, g) =
        fmap (\f_' -> (a, b, c, d, e, f_', g)) (f f_)


------------------------------------------------------------------------------
instance Functor f => Has VI (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d, e, f_', g, h)
    f_
    f_'
  where
    has _ f (a, b, c, d, e, f_, g, h) =
        fmap (\f_' -> (a, b, c, d, e, f_', g, h)) (f f_)


------------------------------------------------------------------------------
instance Functor f => Has VI (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e, f_', g, h, i)
    f_
    f_'
  where
    has _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\f_' -> (a, b, c, d, e, f_', g, h, i)) (f f_)


------------------------------------------------------------------------------
instance Functor f => Has VII (->) f
    (a, b, c, d, e, f_, g)
    (a, b, c, d, e, f_, g')
    g
    g'
  where
    has _ f (a, b, c, d, e, f_, g) =
        fmap (\g' -> (a, b, c, d, e, f_, g')) (f g)


------------------------------------------------------------------------------
instance Functor f => Has VII (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d, e, f_, g', h)
    g
    g'
  where
    has _ f (a, b, c, d, e, f_, g, h) =
        fmap (\g' -> (a, b, c, d, e, f_, g', h)) (f g)


------------------------------------------------------------------------------
instance Functor f => Has VII (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e, f_, g', h, i)
    g
    g'
  where
    has _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\g' -> (a, b, c, d, e, f_, g', h, i)) (f g)


------------------------------------------------------------------------------
instance Functor f => Has VIII (->) f
    (a, b, c, d, e, f_, g, h)
    (a, b, c, d, e, f_, g, h')
    h
    h'
  where
    has _ f (a, b, c, d, e, f_, g, h) =
        fmap (\h' -> (a, b, c, d, e, f_, g, h')) (f h)


------------------------------------------------------------------------------
instance Functor f => Has VIII (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e, f_, g, h', i)
    h
    h'
  where
    has _ f (a, b, c, d, e, f_, g, h, i)
        = fmap (\h' -> (a, b, c, d, e, f_, g, h', i)) (f h)


------------------------------------------------------------------------------
instance Functor f => Has IX (->) f
    (a, b, c, d, e, f_, g, h, i)
    (a, b, c, d, e, f_, g, h, i')
    i
    i'
  where
    has _ f (a, b, c, d, e, f_, g, h, i) =
        fmap (\i' -> (a, b, c, d, e, f_, g, h, i')) (f i)


------------------------------------------------------------------------------
_1 :: Has I p f s t a b => p a (f b) -> p s (f t)
_1 = has (Proxy :: Proxy I)


------------------------------------------------------------------------------
_2 :: Has II p f s t a b => p a (f b) -> p s (f t)
_2 = has (Proxy :: Proxy II)


------------------------------------------------------------------------------
_3 :: Has III p f s t a b => p a (f b) -> p s (f t)
_3 = has (Proxy :: Proxy III)


------------------------------------------------------------------------------
_4 :: Has IV p f s t a b => p a (f b) -> p s (f t)
_4 = has (Proxy :: Proxy IV)


------------------------------------------------------------------------------
_5 :: Has V p f s t a b => p a (f b) -> p s (f t)
_5 = has (Proxy :: Proxy V)


------------------------------------------------------------------------------
_6 :: Has VI p f s t a b => p a (f b) -> p s (f t)
_6 = has (Proxy :: Proxy VI)


------------------------------------------------------------------------------
_7 :: Has VII p f s t a b => p a (f b) -> p s (f t)
_7 = has (Proxy :: Proxy VII)


------------------------------------------------------------------------------
_8 :: Has VIII p f s t a b => p a (f b) -> p s (f t)
_8 = has (Proxy :: Proxy VIII)


------------------------------------------------------------------------------
_9 :: Has IX p f s t a b => p a (f b) -> p s (f t)
_9 = has (Proxy :: Proxy IX)
