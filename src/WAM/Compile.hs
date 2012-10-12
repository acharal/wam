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
import Debug.Trace


import Control.Monad
import Control.Monad.Reader


type WAMCompile m a = ReaderT WamCompEnv m a

type WamSymbolTable = [(String,WamRegister)]

data WamCompEnv = WamCompEnv 
    {   perms'    :: [String]       -- ^ permanent variables
    ,   unsafe'   :: [String]       -- ^ unsafe variables
    ,   firstFree :: Int            -- ^ Maximum number of reserved variables 
    ,   symbolTbl :: WamSymbolTable -- ^ mapping between clause variables and wamregisters
    }


-- | Returns the permanent variables from a clause
-- | permanent variables are computed as described below : 
-- | those that exist in more that one body literals
-- | assuming that the head literal and the first body literal are considered as one
-- | body literal.
perms :: Clause -> [String]
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
safe :: Clause -> [String]
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

-- | Returns the "unsafe" variables
unsafe :: Clause -> [String]
unsafe c@(h,b) = (varsClause c) \\ safe c


-- | extendTable 
extendTable :: [Term]               -- ^ a list of terms
            -> WamSymbolTable       -- ^ current symbol table
            -> [String]             -- ^ permanent variables
            -> Int                  -- ^ minimum free number
            -> WamSymbolTable       -- ^ returns new extended symbol table
extendTable ts tbl perms n =
    let extendTableAux [] [] _ tbl = tbl
        extendTableAux [] (t:ts) (r:rs) tbl =
            case t of
                T (s, args) -> 
                    extendTableAux args ts rs tbl
                V v -> 
                    if v `inTable` tbl 
                    then extendTableAux [] ts rs tbl
                    else extendTableAux [] ts rs (extendTblNewVar tbl perms (r:rs) n v)
        extendTableAux (a:as) ts rs tbl =
             case a of
                T (_, []) -> 
                    extendTableAux as ts rs tbl
                T (_, args) -> 
                    extendTableAux as (a:ts) ((newTempVar tbl rs n):rs) tbl
                V v -> 
                    if v `inTable` tbl 
                    then extendTableAux as ts rs tbl
                    else extendTableAux as ts rs (extendTblNewVar tbl perms rs n v)
        inTable v tbl = v `elem` (map fst tbl)
    in extendTableAux [] ts xs tbl
    where xs = map Temp $ reverse [1..length ts]

-- | extend symbol table with a new variable
-- | if the variable belongs to the permanent variables then we extend the 
-- | the symbol table with a new permanent register. if not then we extend it with
-- | a new temp register.
extendTblNewVar :: WamSymbolTable       -- ^ current symbol table
                -> [String]             -- ^ permanents variables
                -> [WamRegister]        -- ^ reserved registers
                -> Int                  -- ^ minimum free number
                -> String               -- ^ name of variable
                -> WamSymbolTable       -- ^ returns new symbol table
extendTblNewVar tbl perms r n v | v `elem` perms = (v, newPermVar tbl):tbl
                                | otherwise      = (v, newTempVar tbl r n):tbl

unWrapVar (Temp i) = i
unWrapVar (Perm i) = i

isPerm (Perm _) = True
isPerm _ = False

isTemp v = not (isPerm v)

newVar tbl perms r n v =
    let tbl' = extendTblNewVar tbl perms r n v
    in  (fromJust (lookup v tbl'), tbl')

newPermVar tbl = Perm (n + 1)
    where p = map unWrapVar $ filter isPerm $ map snd tbl
          n = case p of
                [] -> 0
                _  -> maximum p

newTempVar tbl r m = Temp (n + 1)
    where t = filter isTemp $ map snd tbl
          p = map unWrapVar $ t ++ r
          n = case p of
                 [] -> m
                 _  -> max m (maximum p)


-- | Converts a term into a label. eg p(a,b,c) to p/3
termToLabel :: Term -> WamLabel 
termToLabel (T (s, args)) = (s, length args)
termToLabel _ = error "cannot convert to wamlabel"

-- WAM Compilation

-- | Compiles a literal, either a head literal or a body literal
wamCompileLit :: Bool               -- ^  h is a bool - if true then compilation is a "get" else is a "put" mode
              -> [Term]             -- ^  a list of literals to compile
              -> [WamRegister]      -- ^  a list of wam registers to assign to literals (one register for one literal)
              -> WamSymbolTable     -- ^  the mapping between the name of a variable and the wamregister that referenced to it
              -> [String]           -- ^  the set of permanent variables (scope clause)
              -> [String]           -- ^  the set of unsafe variables (scope clause)
              -> Int                -- ^  a maximum integer used to assign new variables 
              -> WamInstrSeq        -- ^  the output sequence of wam instructions
wamCompileLit h [] _ _ _ _ _ = []
wamCompileLit h (t:ts) (r:rs) tbl perms u n =
    let
       opValue     = if h then GetValue     else PutValue
       opConstant  = if h then GetConstant  else PutConstant
       opStructure = if h then GetStructure else PutStructure
       opVariable  = if h then GetVariable  else PutVariable
    in case t of
         T (s, []) -> 
            (opConstant s,[r]) : (wamCompileLit h ts rs tbl perms u n)
         T (_, args) -> 
            case r of
                Temp i -> 
                    if i > n 
                    then (GetStructure (termToLabel t), [r]) : (wamCompileTerm h args ts rs tbl perms u n)
                    else (opStructure  (termToLabel t), [r]) : (wamCompileTerm h args ts rs tbl perms u n)
                _ -> 
                         (opStructure  (termToLabel t), [r]) : (wamCompileTerm h args ts rs tbl perms u n)
         V v -> 
            case lookup v tbl of
               Just z -> 
                    if  v `elem` u
                    then (PutUnsafeValue, [z,r]) : (wamCompileLit h ts rs tbl perms (delete v u) n)
                    else (opValue,        [z,r]) : (wamCompileLit h ts rs tbl perms u n)
               Nothing -> 
                    let (z,tbl') = newVar tbl perms (r:rs) n v
                    in (opVariable, [z,r]) : (wamCompileLit h ts rs tbl' perms u n)


-- | Compiles head literals
wamCompileHeadLit ts perms n = wamCompileLit True ts xs [] perms [] n
   where n' = length ts
         xs = map Temp $ reverse [1..n']


-- | Compiles a goal literal
wamCompileGoalLit ts tbl perms u = wamCompileLit False ts xs tbl perms u n
   where n  = length ts
         xs = map Temp $ reverse [1..n]

-- | Compiles a term
wamCompileTerm :: Bool               -- ^  h is a bool - if true then compilation is a "get" else is a "put" mode
               -> [Term]             -- ^  a list of terms to compile
               -> [Term]             -- ^  a list of literals to continue compilation after the compilation of the first argument
               -> [WamRegister]      -- ^  a list of wam registers to assign to literals (one register for one literal)
               -> WamSymbolTable     -- ^  the mapping between the name of a variable and the wamregister that referenced to it
               -> [String]           -- ^  the set of permanent variables (scope clause)
               -> [String]           -- ^  the set of unsafe variables (scope clause)
               -> Int                -- ^  a maximum integer used to assign new variables 
               -> WamInstrSeq        -- ^  the output sequence of wam instructions
wamCompileTerm h [] ts rs tbl perms u n =  wamCompileLit h ts rs tbl perms u n
wamCompileTerm h (a:as) ts rs tbl perms u n =
    case a of
       T (s,[]) -> 
            (UnifyConstant s, []):(wamCompileTerm h as ts rs tbl perms u n)
       T (s, args) -> 
            let r' = newTempVar tbl rs n
            in (UnifyVariable, [r']):(wamCompileTerm h as (a:ts) (r':rs) tbl perms u n)
       V v ->
            case lookup v tbl of
                Just z -> 
                    (UnifyValue, [z]):(wamCompileTerm h as ts rs tbl perms u n)
                Nothing -> 
                    let (z, tbl') = newVar tbl perms rs n v
                    in (UnifyVariable, [z]):(wamCompileTerm h as ts rs tbl' perms u n)

-- | Compiles the body consisted of many body literals
wamCompileBody [] _ _ _ _ = [(Proceed, [])]
wamCompileBody [g] tbl perms u e =
   let c' = if e 
            then (Deallocate,[]):c 
            else c
           where c = [(Execute (termToLabel g), [])]
   in  wamCompileGoalLit (args g) tbl perms u ++ c'
wamCompileBody (g:gs) tbl perms u e =
   let as   = args g
       c    = wamCompileGoalLit as tbl perms u ++ [(Call (termToLabel g), [])]

       n    = length as

       -- symbol table is extended only by the permanent variables
       tbl' = filter (isPerm.snd) $ extendTable as tbl perms n
   in c `mplus` (wamCompileBody gs tbl' perms u e)


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

        g' = if notSingleton 
             then (Allocate (length permans), []):g 
             else g
            where g = wamCompileHeadLit headArgs permans n

        permans = perms cl
        unsafes = unsafe cl

        -- initial symbol table is extended by the variables of the head arguments.
        -- we assume that extendTable assigns registers to variables exactly like wamCompileHeadLit
        tbl     = extendTable headArgs [] permans n

   in   g' `mplus` (wamCompileBody b tbl permans unsafes notSingleton)

-- wamCompileGoal

-- | Compiles the many alternative clauses of a predicate
wamCompileAlters (l:ls) i =
   let c = wamCompileClause l
   in case ls of
        [] -> (TrustMe,[]):c
        _ ->  let c' = (RetryMeElse j,[]):c
                  j  = i + length c'
              in c' `mplus` wamCompileAlters ls j

-- | Compiles a whole predicate consisting of none or many alternatives
wamCompilePredicate []     i = [(Backtrack, [])]
wamCompilePredicate [d]    i = wamCompileClause d
wamCompilePredicate (d:ds) i =
   let c  = wamCompileClause d
       c' = (TryMeElse j, []):c
       j  = i + length c'
   in c' `mplus` wamCompileAlters ds j

-- | Compiles the definitions of the predicates
wamCompileDefs :: [WamLabel]      -- ^ list of predicate names to compile
               -> [Clause]      -- ^ clauses of program
               -> Int           -- ^ offset to start
               -> [WamInstrSeq] -- ^ returns a list of instruction sequence, one for each predicate
wamCompileDefs [] p i = []
wamCompileDefs (q:qs) p i =
   let ds = defs p q
       c  = wamCompilePredicate ds i
       j  = i + length c
   in  c : (wamCompileDefs qs p j)

-- | Compiles a logic program consisting of many definitions
wamCompileProg :: [Clause] 
               -> WamProgram
wamCompileProg p =
   let ps = preds p
       i  = 1
       cs = wamCompileDefs ps p i
   in mkDB $ zip ps cs

wamCompileGoal :: Goal 
               -> Int 
               -> WamGoal
wamCompileGoal g i = 
    let g'  = (T ("?", vg'), g)
        vg' = map V $ vg
        vg  = varsGoal g
    in (reverse vg, wamCompilePredicate [g'] i)


