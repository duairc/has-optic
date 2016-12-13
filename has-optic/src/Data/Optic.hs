{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

#ifdef DataPolyKinds
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#ifdef SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Optic
    ( Optic
    , Has
    , optic'
#ifdef AmbiguousTypes
    , optic
#endif
    , module Data.Optic.Accessors
    )
where

-- has-optic -----------------------------------------------------------------
import           Data.Optic.Accessors
import           Data.Optic.Instances.Anonymous ()
import           Data.Optic.Instances.Functor ()
import           Data.Optic.Instances.Monoid ()
import           Data.Optic.Instances.Sum ()
import           Data.Optic.Instances.Tuple ()


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core
                     ( Has
                     , Optic
                     , optic'
#ifdef AmbiguousTypes
                     , optic
#endif
                     )
