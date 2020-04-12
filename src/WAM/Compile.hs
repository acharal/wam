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
   , wamCompileGoal
   ) where

import Prolog
import WAM
import System.IO
import Data.List (nub, delete, (\\))
import Data.Maybe (fromJust)


import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import Debug.Trace

type WamCompile a = ReaderT WamCompEnv (StateT WamCompState Identity) a

type WamSymbolTable = [(VarId,WamRegister)]

data WamCompEnv = WamCompEnv
    {   perms'    :: [VarId]       -- ^ permanent variables
    }

data WamCompState = WamCompState
    {   symbolTbl :: WamSymbolTable -- ^ mapping between clause variables and wamregisters
    ,   unsafe'   :: [VarId]        -- ^ unsafe variables
    }

-- | Returns the permanent variables from a clause
-- | permanent variables are computed as described below :
-- | those that exist in more that one body literals
-- | assuming that the head literal and the first body literal are considered as one
-- | body literal.
perms :: Clause -> [VarId]
perms (t, ts) =
    let varsHead = varsTerm t
        varsBody = map varsTerm ts
        lst = case varsBody of
                  [] -> [varsHead]
                  (v:vs) -> (nub (varsHead ++ v)):vs
        aux [] = []
        aux (l:ls) = (filter (`elem` l') l) ++ aux ls
            where l' = concat ls
    in nub $ aux lst

-- | Returns the "safe" variables
-- | safe variables are computed as described below
-- | - the variables in the head literal is safe by default
-- | - variables in a compound literal (inside term structures) are safe
-- | - variables that do not occur only in the last literal of the body.
safe :: Clause -> [VarId]
safe (h, []) = varsTerm h
safe (h, b)  = nub $ varsTerm h  ++ varsNotL ++ varsInCompound
    where b' = init b
          l  = last b
          vl = varsTerm l
          varsNotL = varsClause (h,b) \\ vl
          varsInCompound = concat $ inCompound $ concatMap args (h:b)

          inCompound [] = []
          inCompound (t:ts) =
                case t of
                    T (s,args) ->
                        nub $ map varsTerm args ++ inCompound ts
                    V _ ->
                        inCompound ts
                    V2 (v, args) ->
                        nub $ map varsTerm args ++ inCompound ts

-- | Returns the "unsafe" variables
unsafe :: Clause -> [VarId]
unsafe c@(h,b) = (varsClause c) \\ safe c

unWrapVar (Temp i) = i
unWrapVar (Perm i) = i

isPerm (Perm _) = True
isPerm _ = False

isTemp v = not (isPerm v)

newVar r n v = do

    pe <- asks perms'
    p  <- if v `elem` pe then
             newPerm
          else
             newTemp r n

    tbl <- gets symbolTbl

    let tbl' = (v,p):tbl

    modify (\st -> st { symbolTbl = tbl' })

    return p

newPerm = do
        tbl <- gets symbolTbl
        let p = map unWrapVar $ filter isPerm $ map snd tbl
        let n = case p of
                  [] -> 0
                  _  -> maximum p
        return $ Perm (n + 1)

newTemp r m = do
        tbl <- gets symbolTbl
        let t = filter isTemp $ map snd tbl
        let p = map unWrapVar $ t ++ r
        let n = case p of
                   [] -> m
                   _  -> max m (maximum p)
        return $ Temp (n + 1)

-- | Converts a term into a label. eg p(a,b,c) to p/3
termToLabel :: Term -> WamLabel
termToLabel (T (s, args)) = (s, length args)
termToLabel _ = error "cannot convert to wamlabel"

-- WAM Compilation

-- | Compiles a literal, either a head literal or a body literal
wamCompileLit :: Bool               -- ^  h is a bool - if true then compilation is a "get" else is a "put" mode
              -> [Term]             -- ^  a list of literals to compile
              -> [WamRegister]      -- ^  a list of wam registers to assign to literals (one register for one literal)
              -> Int                -- ^  a maximum integer used to assign new variables
              -> WamCompile WamInstrSeq  -- ^  the output sequence of wam instructions
wamCompileLit h [] _ _  = return []
wamCompileLit h (t:ts) (r:rs) n =
    let
       opValue     = if h then GetValue     else PutValue
       opConstant  = if h then GetConstant  else PutConstant
       opStructure = if h then GetStructure else PutStructure
       opVariable  = if h then GetVariable  else PutVariable
    in case t of
         T (s, []) -> do
            rest <- wamCompileLit h ts rs n
            return $ (opConstant s, [r]) : rest
         T (_, args) -> do
            str  <- case r of
                     Temp i -> do
                        if i > n -- not an argument Temp 1...Temp n is reserved for procedural calls
                        then return (GetStructure (termToLabel t), [r])
                        else return (opStructure  (termToLabel t), [r])
                     _ -> do
                             return (opStructure  (termToLabel t), [r])
            rest <- wamCompileTerm h args ts rs n
            return (str:rest)
         V v -> do
            tbl <- gets symbolTbl
            case lookup v tbl of
               Just z -> do
                    u <- gets unsafe'
                    if  v `elem` u
                    then do
                        modify (\st -> st{unsafe' = (delete v u)})
                        rest <- wamCompileLit h ts rs n
                        return $ (PutUnsafeValue, [z,r]) : rest
                    else do
                        rest <- wamCompileLit h ts rs n
                        return $ (opValue,        [z,r]) : rest
               Nothing -> do
                    z <- newVar (r:rs) n v
                    rest <- wamCompileLit h ts rs n
                    return $ (opVariable, [z,r]) : rest
         V2 (v, args) -> do
            tbl <- gets symbolTbl
            case lookup v tbl of
               Just z -> do
                    rest <- wamCompileTerm h args ts rs n
                    return $ (PutApplication (length args), [z, r]) : rest
               Nothing -> do
                    z <- newVar (r:rs) n v
                    rest <- wamCompileTerm h args ts rs n
                    return $ [
                            (PutVariable, [z, z]),
                            (PutApplication (length args), [z, r])
                        ] ++ rest

-- | Compiles head literals
wamCompileHeadLit ts n = wamCompileLit True ts xs n
   where n' = length ts
         xs = map Temp [1..n']

-- | Compiles a goal literal
wamCompileGoalLit ts = wamCompileLit False ts xs n
   where n  = length ts
         xs = map Temp [1..n]

-- | Compiles a term
wamCompileTerm :: Bool               -- ^  h is a bool - if true then compilation is a "get" else is a "put" mode
               -> [Term]             -- ^  a list of terms to compile
               -> [Term]             -- ^  a list of literals to continue compilation after the compilation of the first argument
               -> [WamRegister]      -- ^  a list of wam registers to assign to literals (one register for one literal)
               -> Int                -- ^  a minimum lower bound integer used to assign new variables
               -> WamCompile WamInstrSeq        -- ^  the output sequence of wam instructions
wamCompileTerm h [] ts rs n     =  wamCompileLit h ts rs n
wamCompileTerm h (a:as) ts rs n =
    case a of
       T (s,[]) -> do
            rest <- wamCompileTerm h as ts rs n
            return $ (UnifyConstant s, []) : rest
       T (s, args) -> do
            r' <- newTemp rs n
            rest <- wamCompileTerm h as (a:ts) (r':rs) n
            return $ (UnifyVariable, [r']) : rest
       V v -> do
            tbl <- gets symbolTbl
            case lookup v tbl of
                Just z -> do
                    rest <- wamCompileTerm h as ts rs n
                    return $ (UnifyValue, [z]) : rest
                Nothing -> do
                    z <- newVar rs n v
                    rest <- wamCompileTerm h as ts rs n
                    return $ (UnifyVariable, [z]):rest
       V2 (v, args) -> do
            tbl <- gets symbolTbl
            case lookup v tbl of
                Just z -> do
                    rest <- wamCompileTerm h as (a:ts) (z:rs) n
                    return $ (UnifyValue, [z]) : rest
                Nothing -> do
                    z <- newVar rs n v
                    rest <- wamCompileTerm h as (a:ts) (z:rs) n
                    return $ (UnifyVariable, [z]) : rest

-- | Compiles the body consisted of many body literals
wamCompileBody [] _  = return [(Proceed, [])]
wamCompileBody [g] e = do
     cc <- case g of
        T _ -> wamCompileGoalLit (args g)
        V v -> wamCompileGoalLit [g]
        V2 _ -> wamCompileGoalLit [g]
     c <- case g of
        T _       -> return $ [(Execute (termToLabel g), [])]
        V v       -> return $ [(ExecuteVariable, [Temp 1])]
        V2 (v, _) -> return $ [(ExecuteVariable, [Temp 1])]
     let c' = if e then (Deallocate,[]):c else c

     return $ cc ++ c'

wamCompileBody (g:gs) e = do
      prologue <- case g of
        T _ -> wamCompileGoalLit (args g)
        V v -> wamCompileGoalLit [g]
        V2 _ -> wamCompileGoalLit [g]
      c <- case g of
        T _       -> return $ [(Call (termToLabel g), [])]
        V v       -> return $ [(CallVariable, [Temp 1])]
        V2 (v, _) -> return $ [(CallVariable, [Temp 1])]

      modify (\st -> st{symbolTbl = filter (isPerm.snd) (symbolTbl st)})
      cc <- wamCompileBody gs e

      return (prologue ++ c ++ cc)


-- | Compiles a clause
wamCompileClause cl@(h,b) =
   let
        -- n registers are reserved for arguments of first literal in body (?)
        n = if isFact
            then 0
            else length (args (head b))

        isFact       = length b < 1
        notSingleton = length b > 1

        headArgs = args h

        permans = perms cl
        unsafes = unsafe cl

   in do

        local (\r -> r{ perms' = permans }) $ do
            modify (\st -> st { symbolTbl = []
                              , unsafe'   = unsafes
                              })

            g <- wamCompileHeadLit headArgs n

            let g'= if notSingleton then (Allocate (length permans), []):g else g

            gb <- wamCompileBody b notSingleton
            return $ g' ++ gb

-- wamCompileGoal

-- | Compiles the many alternative clauses of a predicate
wamCompileAlters (l:ls) i = do
    c <- wamCompileClause l
    case ls of
        [] -> do
            return $ (TrustMe,[]):c
        _ ->  do
            let j  = i + length c + 1
            let c' = (RetryMeElse j,[]):c
            alters <- wamCompileAlters ls j
            return $ c' ++ alters


-- | Compiles a whole predicate consisting of none or many alternatives
wamCompilePredicate []     i = return [(Backtrack, [])]
wamCompilePredicate [d]    i = wamCompileClause d
wamCompilePredicate (d:ds) i = do
   c  <- wamCompileClause d
   let j  = i + length c + 1
   let c' = (TryMeElse j, []):c
   alters <- wamCompileAlters ds j
   return $ c' ++ alters

-- | Compiles the definitions of the predicates
wamCompileDefs :: [WamLabel]      -- ^ list of predicate names to compile
               -> [Clause]      -- ^ clauses of program
               -> Int           -- ^ offset to start
               -> WamCompile [WamInstrSeq] -- ^ returns a list of instruction sequence, one for each predicate
wamCompileDefs [] p i = return []
wamCompileDefs (q:qs) p i = do
       c <- wamCompilePredicate (defs p q) i
       let j  = i + length c
       defs <- wamCompileDefs qs p j
       return $ c : defs

wamCompile m = runIdentity (evalStateT (runReaderT m emptyEnv) emptyState)
    where emptyState = WamCompState { symbolTbl = [], unsafe' = [] }
          emptyEnv   = WamCompEnv   { perms' = [] }

-- | Compiles a logic program consisting of many definitions
wamCompileProg :: [Clause]
               -> WamProgram
wamCompileProg p =
   let ps = preds p
       i  = 1
       cs = wamCompile (wamCompileDefs ps p i)
   in mkDB $ zip ps cs

wamCompileGoal :: Goal
               -> Int
               -> WamGoal
wamCompileGoal g i =
    let g'  = (T ("?", vg'), g)
        vg' = map V $ vg
        vg  = varsGoal g
    in (vg, wamCompile (wamCompilePredicate [g'] i))


