-----------------------------------------------------------------------------
--
-- Module      :  WAM.Compile
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

module WAM.Compile (
    wamCompileProg
) where

import Prolog
import WAM
import System.IO
import Data.List (nub, delete, (\\))
import Data.Maybe (fromJust)
import Debug.Trace


import Control.Monad
import Control.Monad.Trans.State

-- type WAMCompile m a = StateT WamCompileState m a


perms :: Clause -> [String]
perms (t, ts) =
    let headvars = varsTerm t
        bodyvarlst = map varsTerm ts
        lst = case bodyvarlst of
                  [] -> [headvars]
                  (t:ts) -> (nub (headvars ++ t)):ts
        aux [] = []
        aux (l:ls) = (filter (\e -> e `elem` l') l) ++ aux ls where l' = concat ls
    in nub $ aux lst

safe (h, []) = varsTerm h
safe (h, b) = nub $ varsTerm h  ++ varsNotL ++ varsInCompound
    where b' = init b
          l  = last b
          vl = (varsTerm l)
          varsNotL = varsClause (h,b) \\ vl
          varsInCompound = concat $ inCompound $ concatMap (\(T (_,a)) -> a) (h:b)
          inCompound [] = []
          inCompound (t:ts) =  case t of
                    T (s,args) -> nub $ map varsTerm args ++ inCompound ts
                    V _ -> inCompound ts


unsafe :: Clause -> [String]
unsafe c@(h,b) = (varsClause c) \\ safe c

type WamSymbolTable = [(String,WamRegister)]

extendTable ts tbl perms n =
    let extendTableAux [] [] _ tbl = tbl
        extendTableAux [] (t:ts) (r:rs) tbl =
            case t of
                T (s, args) -> extendTableAux args ts rs tbl
                V v -> if v `inTable` tbl then
                           extendTableAux [] ts rs tbl
                       else
                           extendTableAux [] ts rs (extendTblNewVar tbl perms (r:rs) n v)
        extendTableAux (a:as) ts rs tbl =
             case a of
                T (_, []) -> extendTableAux as ts rs tbl
                T (_, args) -> extendTableAux as (a:ts) ((newTempVar tbl rs n):rs) tbl
                V v -> if v `inTable` tbl then
                           extendTableAux as ts rs tbl
                       else
                           extendTableAux as ts rs (extendTblNewVar tbl perms rs n v)
        inTable v tbl = v `elem` (map fst tbl)
    in extendTableAux [] ts xs tbl
    where xs = map (\i -> Temp i) (reverse [1..length ts])

extendTblNewVar tbl perms r n v | (v `elem` perms) = (v, newPermVar tbl):tbl
                                | otherwise = (v, newTempVar tbl r n):tbl

newVar tbl perms r n v =
    let tbl' = extendTblNewVar tbl perms r n v
    in  (fromJust (lookup v tbl'), tbl')

newPermVar tbl = Perm (n + 1)
    where n  = case p of
                  [] -> 0
                  _  -> maximum p
          p = map (\(Perm i) -> i) $ filter isPerm $ map (snd) tbl
          isPerm (Perm _) = True
          isPerm _ = False

newTempVar tbl r m = Temp (n + 1)
    where isTemp (Temp _) = True
          isTemp _ = False
          t = filter isTemp $ map (snd) tbl
          p = map (\(Temp i) -> i) (t ++ r)
          n = case p of
                 [] -> m
                 _ -> max m (maximum p)

-- WAM Compilation

wamCompileLit h [] _ _ _ _ _ = []
wamCompileLit h (t:ts) (r:rs) tbl perms u n =
    let
       opValue = if h then GetValue else PutValue
       opConstant = if h then GetConstant else PutConstant
       opStructure = if h then GetStructure else PutStructure
       opVariable = if h then GetVariable else PutVariable
    in case t of
         T (s, []) -> (opConstant s,[r]) : (wamCompileLit h ts rs tbl perms u n)
         T (s, args) -> case r of
                         (Temp i) ->
                              if i > n then
                                   (GetStructure (s,length args), [r]):(wamCompileTerm h args ts rs tbl perms u n)
                              else
                                   (opStructure (s,length args), [r]) : (wamCompileTerm h args ts rs tbl perms u n)
                         _ -> (opStructure (s,length args), [r]) : (wamCompileTerm h args ts rs tbl perms u n)
         (V v) -> case lookup v tbl of
                     Just z -> if (v `elem` u) then
                                    (PutUnsafeValue, [z,r]) : (wamCompileLit h ts rs tbl perms (delete v u) n)
                               else
                                    (opValue, [z,r]) : (wamCompileLit h ts rs tbl perms u n)
                     Nothing -> let (z,tbl') = newVar tbl perms (r:rs) n v
                                in (opVariable, [z,r]) : (wamCompileLit h ts rs tbl' perms u n)

wamCompileHeadLit ts perms n = wamCompileLit True ts xs [] perms [] n
   where n' = length ts
         xs = map (\i -> Temp i) (reverse [1..n'])

wamCompileGoalLit ts tbl perms u = wamCompileLit False ts xs tbl perms u n
   where n = length ts
         xs = map (\i -> Temp i) (reverse [1..n])

wamCompileTerm h [] ts rs tbl perms u n =  wamCompileLit h ts rs tbl perms u n
wamCompileTerm h (a:as) ts rs tbl perms u n =
    case a of
       (T (s,[])) -> (UnifyConstant s, []):(wamCompileTerm h as ts rs tbl perms u n)
       (T (s, args)) -> let r' = newTempVar tbl rs n
                        in (UnifyVariable, [r']):(wamCompileTerm h as (a:ts) (r':rs) tbl perms u n)
       (V v) -> case lookup v tbl of
                     Just z -> (UnifyValue, [z]):(wamCompileTerm h as ts rs tbl perms u n)
                     Nothing -> let (z, tbl') = newVar tbl perms rs n v
                                in (UnifyVariable, [z]):(wamCompileTerm h as ts rs tbl' perms u n)

wamCompileBody [] _ _ _ _ = [(Proceed, [])]
wamCompileBody [g] tbl perms u e =
   let (T (s,args)) = g
       c = [(Execute (s,length args), [])]
       c' = if e then (Deallocate,[]):c else c
   in  wamCompileGoalLit args tbl perms u ++ c'
wamCompileBody (g:gs) tbl perms u e =
   let (T (s,args)) = g
       c = wamCompileGoalLit args tbl perms u ++ [(Call (s,length args), [])]
       n = length args
       isPerm (_,(Perm _)) = True
       isPerm _ = False
       tbl' = filter isPerm $ extendTable args tbl perms n
   in c ++ (wamCompileBody gs tbl' perms u e)

wamCompileClause (h,b) =
   let  has = args h
        args (T (_, as)) = as
        n = if length b < 1 then 0 else (length (args (head b)))
        p = (perms (h,b))
        g = wamCompileHeadLit has p n
        e = length b >= 2
        tbl = extendTable has [] p n
        g' = if (e) then (Allocate (length p), []):g else g
        u = unsafe (h,b)
   in   g' ++ (wamCompileBody b tbl p u e)

-- wamCompileGoal


wamCompileAlters (l:ls) i =
   let c = wamCompileClause l
   in case ls of
        [] -> (TrustMe,[]):c
        _ -> let j = (length c) + i + 1
                 c' = (RetryMeElse j,[]):c
             in c' ++ wamCompileAlters ls j

wamCompilePredicate [] i = [(Backtrack, [])]
wamCompilePredicate [d] i = wamCompileClause d
wamCompilePredicate (d:ds) i =
   let c = wamCompileClause d
       j = length c + i + 1
       c' = (TryMeElse j, []):c
   in c' ++ wamCompileAlters ds j


wamCompileDefs [] p i = []
wamCompileDefs (q:qs) p i =
   let ds = (defs p q)
       c  = (wamCompilePredicate ds i)
   in c:(wamCompileDefs qs p (length c + i))

wamCompileProg (g,p) =
   let ps = (gp, length vg):preds p
       ps' = map fst ps
       i = 1
       vg = varsGoal g
       gp = "?"
       g' = (T (gp, map V $ vg), g)
       cs = wamCompileDefs ps' (g':p) i
       a :: Int -> [Int] -> [Int]
       a start [_] = [start]
       a start (n:x) = start:(a (start + n) x)
   in P (reverse vg) (zip ps $ a 1 (map length cs)) (concat cs)

