{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Optic.TH
    ( accessor
    , accessor_
    , accessors
    )
where

-- base ----------------------------------------------------------------------
import           Data.Foldable (fold)


-- has-optic-core ------------------------------------------------------------
import           Data.Optic.Core (Has, Optic, optic')


-- template-haskell ----------------------------------------------------------
import           Language.Haskell.TH
                     ( Body (NormalB)
                     , Clause (Clause)
                     , Dec (FunD, SigD)
                     , Exp (AppE, VarE)
#if !MIN_VERSION_template_haskell(2, 10, 0)
                     , Pred (ClassP)
#endif
                     , Q
                     , Type (AppT, ConT, ForallT, VarT)
                     , TyVarBndr (PlainTV)
                     , mkName
                     )


-- types-th ------------------------------------------------------------------
import           Type.TH (proxy, string)


------------------------------------------------------------------------------
accessor :: String -> Type -> Q [Dec]
accessor name key = do
    let vars = map mkName ["p", "f", "s", "t", "a", "b"]
    let sig = SigD (mkName name) (ForallT
         (map PlainTV vars)
#if MIN_VERSION_template_haskell(2, 10, 0)
         [foldl AppT ((AppT (ConT ''Has) key)) (map VarT vars)]
#else
         [ClassP ''Has (key : map VarT vars)]
#endif
         (foldl AppT (ConT ''Optic) (map VarT vars)))
    let fun = FunD (mkName name)
         [Clause [] (NormalB (AppE (VarE 'optic') (proxy key))) []]
    return [sig, fun]


------------------------------------------------------------------------------
accessor_ :: String -> Q [Dec]
accessor_ name = accessor name (string name)


------------------------------------------------------------------------------
accessors :: [(String, String)] -> Q [Dec]
accessors = fmap fold . traverse (\(a, b) -> accessor a (string b))
