module Converter.TH where

import ClassyPrelude

import Language.Haskell.TH

-- |Returns True iff two ADTs have the same constructor.
--
--  Intended code, with @(A1,...,An)@ being the constructors
--  of @a@:
--
-- @
-- isSame A1{} A1{} = True
-- ...
-- isSame An{} An{} = True
-- isSame _    _    = False
-- @
isSame :: Name -> ExpQ
isSame xname = do
   info <- reify xname

   return $ case info of
      (TyConI (DataD _ _ _ _ cs _)) -> lam cs
      (TyConI NewtypeD{}) -> true
      _ -> false

   where
      x1 = mkName "x1"
      y1 = mkName "y1"

      true = ConE (mkName "True")
      false = ConE (mkName "False")

      mkTrue (NormalC c _) = Match (TupP [RecP c [], RecP c []]) (NormalB true) []
      mkTrue _ = Match WildP (NormalB false) []
      mkFalse = Match WildP (NormalB false) []

      lam cs =
         LamE [VarP x1, VarP y1]
              (CaseE (TupE [VarE x1, VarE y1])
                     (map mkTrue cs ++ [mkFalse]))
