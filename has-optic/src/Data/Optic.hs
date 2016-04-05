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
    )
where

-- has-optic -----------------------------------------------------------------
--import           Data.Optic.Instances.Anonymous ()
import           Data.Optic.Instances.Tuple ()
import           Data.Optic.TH (accessor)


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core
                     ( Has
                     , Optic
                     , optic'
#ifdef AmbiguousTypes
                     , optic
#endif
                     )


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
