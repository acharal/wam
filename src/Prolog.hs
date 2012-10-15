-----------------------------------------------------------------------------
--
-- Module      :  Prolog
-- Copyright   :
-- License     :  GPL Nothing
--
-- Maintainer  :  Angelos Charalambidis <a.charalambidis@di.uoa.gr>
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Prolog where

import Data.List (nub)

type VarId = String -- deriving (Eq, Show)

data Term    =  
    T (String, [Term]) 
  | V VarId
  | V2 (VarId, [Term]) 
  deriving (Show, Eq)

type Clause  = (Term, [Term])

type Goal    = [Term]

type Program = (Goal,[Clause])

data Type = TypeO | TypeI | F Type Type deriving Show

varsTerm t = nub $ vars' t
    where vars' (T (_, ts)) = concatMap vars' ts
          vars' (V v) = [v]

varsClause (t, ts) = nub $ varsTerm t ++ concatMap varsTerm ts

varsGoal ts = nub $ concatMap varsTerm ts

preds cs =  nub $ map (\(T (s,args),_) -> (s, length args)) cs

defs cs (p,n) =  filter isOfPred cs 
    where isOfPred (T (s,args),_) = s == p && length args == n

args (T (_, x))    = x
functor (T (x, _)) = x

-- types

order (TypeO) = 0
order (TypeI) = 0
order (F t1 t2) = max (1 + (order t1)) (order t2)

typesVarsClause (t, ts) tysig = typesVars t TypeO tysig

-- typesVars :: -> Type ->  [(String, Type)]
typesVars (T (s, args)) ty tysig =
    let argTy (F t1 t2) = t1:(argTy t2)
        argTy _ = []
    in case lookup s tysig of
        Just funty -> concatMap (\(a,t) -> typesVars a t tysig) $ zip args (argTy funty)
        Nothing -> []-- error $ "cannot find type for " ++ s
typesVars (V v) ty tysig = [(v,ty)]

