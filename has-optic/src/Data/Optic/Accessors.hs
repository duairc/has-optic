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

module Data.Optic.Accessors
    ( _1
    , _2
    , _3
    , _4
    , _5
    , _6
    , _7
    , _8
    , _9

    , _Nothing
    , _Just
    , _Left
    , _Right

    , _Nil
    , _Cons
    , _Snoc

    , _Identity
    , _Const
    , _Compose

    , _All
    , _Alt
    , _Any
    , _Dual
    , _Endo
    , _First
    , _Last
    , _Max
    , _Min
    , _Option
    , _Product
    , _Sum
    , _WrapMonoid

    , _Labeled1

    , head
    , tail
    , init
    , last
    )
where

-- base ----------------------------------------------------------------------
import           Prelude (map)


-- has-optic -----------------------------------------------------------------
import           Data.Optic.TH (accessor, accessors)


-- types-th ------------------------------------------------------------------
import           Type.TH (num)


------------------------------------------------------------------------------
$(accessor "_1" (num 1))


------------------------------------------------------------------------------
$(accessor "_2" (num 2))


------------------------------------------------------------------------------
$(accessor "_3" (num 3))


------------------------------------------------------------------------------
$(accessor "_4" (num 4))


------------------------------------------------------------------------------
$(accessor "_5" (num 5))


------------------------------------------------------------------------------
$(accessor "_6" (num 6))


------------------------------------------------------------------------------
$(accessor "_7" (num 7))


------------------------------------------------------------------------------
$(accessor "_8" (num 8))


------------------------------------------------------------------------------
$(accessor "_9" (num 9))


------------------------------------------------------------------------------
$(accessors (map (\s -> ('_':s, s))
    [ "Nothing"
    , "Just"
    , "Left"
    , "Right"
    , "Nil"
    , "Cons"
    , "Snoc"
    , "Identity"
    , "Const"
    , "Compose"
    , "All"
    , "Alt"
    , "Any"
    , "Dual"
    , "Endo"
    , "First"
    , "Last"
    , "Max"
    , "Min"
    , "Option"
    , "Product"
    , "Sum"
    , "WrapMonoid"
    , "Labeled1"
    ]))


------------------------------------------------------------------------------
$(accessors (map (\s -> (s, s))
    [ "head"
    , "tail"
    , "init"
    , "last"
    ]))
