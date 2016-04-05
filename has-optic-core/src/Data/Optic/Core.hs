{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

#ifdef SafeHaskell
{-# LANGUAGE Safe #-}
#endif

#ifdef AmbiguousTypes
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

#ifdef DataPolyKinds
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
#endif

module Data.Optic.Core
    ( Optic
    , Has (optic')
#ifdef AmbiguousTypes
    , optic
#endif
    )
where


------------------------------------------------------------------------------
type Optic p f s t a b = p a (f b) -> p s (f t)


------------------------------------------------------------------------------
class Has n
    (p :: * -> * -> *)
    (f :: * -> *)
    (s :: *)
    (t :: *)
    (a :: *)
    (b :: *)
        | n s -> a
        , n t -> b
        , n s b -> t
        , n t a -> s
  where
    optic' :: proxy n -> Optic p f s t a b


#ifdef AmbiguousTypes
------------------------------------------------------------------------------
data Proxy a = Proxy


------------------------------------------------------------------------------
optic :: forall n p f s t a b. Has n p f s t a b => Optic p f s t a b
optic = optic' (Proxy :: Proxy n)
#endif
