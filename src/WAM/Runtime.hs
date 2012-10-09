-----------------------------------------------------------------------------
--
-- Module      :  WAM.Runtime
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

module WAM.Runtime (
    evalWam,
    wamExecute,
    dumpCell
) where

import WAM
import WAM.Emit
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import System.IO
import Data.Array.IO

data WamCell =
       Struct WamLabel          -- (f/n)
     | Var WamAddress           -- REF n
     | Str WamAddress           -- STR n
     | Cons String              -- CONS s
     | App WamAddress Int       -- APP n m
     | Addr WamAddress
     deriving (Eq, Show)

isVarCell (Var _) = True
isVarCell _ = False

type WamMem  = IOArray WamAddress WamCell
type WamCode = IOArray WamAddress WamInstr


data WamState = WamState { idx :: WamIndex 
                        -- , local :: WamMem
                        -- , heap  :: WamMem
                        -- , trail :: WamMem
                         , mem   :: WamMem        -- ^ global space of memory
                         , code  :: WamCode       -- ^ instructions
                         , regs  :: WamMem        -- ^ registers
                         , reg_p :: WamAddress    -- ^ register pointing  to code
                         , reg_t :: WamAddress    -- ^ register pointing at the top of trail
                         , reg_c :: WamAddress    -- ^ register to hold the last code before a call
                         , reg_h :: WamAddress    -- ^ register pointing at the top of heap (global stack)
                         , reg_b :: WamAddress    -- ^ register pointing at the top of backtrack (local stack)
                         , reg_e :: WamAddress    -- ^ register pointing at the top of the environment (local stack)
                         , reg_a :: Int           -- ^ register holding the arity of the argument
                         , reg_s :: WamAddress    -- ^ structure pointer
                         }

-- type WamRuntime = StateT WamState (ContT () IO)
type WamRuntime = ContT () (StateT WamState IO)

-- memory management

-- | changes the content of the cell in the global memory space
change_cell i c = do
    st <- gets mem
    liftIO $ writeArray st i c

get_cell i = do
    st <- gets mem
    liftIO $ readArray st i

-- | make cell i unbound, namely a variable cell with self-reference
create_unbound i = change_cell i (Var i)

-- | saves the content of a register in the cell in memory
save_reg i r = gets r >>= \a -> change_cell i (Addr a)

-- code control

-- | advance the program register by n
advance n = gets reg_p >>= \p ->  jump (p + n)

-- | jump to a specific address
jump :: WamAddress -> WamRuntime ()
jump p' = modify (\s -> s{reg_p = p'})

-- | read the wam instruction from the current location of p
readinstr n = gets reg_p >>= \p -> gets code >>= \st -> liftIO $ readArray st (n)

set_return_address :: WamRuntime ()
set_return_address = gets reg_p >>= \p -> modify (\s -> s{reg_c = p})

set_structure_pointer :: WamAddress -> WamRuntime ()
set_structure_pointer s = modify (\st -> st{reg_s = s})

proceed :: WamRuntime ()
proceed = gets reg_c >>= \c -> jump c

-- register management

-- | translate address of permanent variable based on environment register
get_perm_real_addr :: WamAddress -> WamRuntime WamAddress
get_perm_real_addr i = do
    e <- gets reg_e
    return (e-1-i)

get_perm :: WamAddress -> WamRuntime WamCell
get_perm i = do
    a <- get_perm_real_addr i
    get_cell a

set_perm :: WamAddress -> WamCell -> WamRuntime ()
set_perm i c = do
    a <- get_perm_real_addr i
    change_cell a c

get_temp i = do
    rs <- gets regs
    liftIO $ readArray rs i

set_temp i c = do
    rs <- gets regs
    liftIO $ writeArray rs i c

-- | get content of a register
get_content (Perm i) = get_perm i
get_content (Temp i) = get_temp i

-- | set content of a register
set_content (Perm i) = set_perm i
set_content (Temp i) = set_temp i


save_arg i a = get_temp a >>= change_cell i

restore_arg i a = get_cell i >>= set_temp a


-- arity management 

set_arity n = modify (\s -> s{reg_a = n})

next_arg :: WamRuntime ()
next_arg = gets reg_s >>= \s -> modify (\st -> st{reg_s = s + 1})


-- trace 

dumpState :: WamRuntime ()
dumpState = do
    h <- gets reg_h
    m <- gets mem
    hs <- liftIO $ mapM (readArray m) [1..h]
    liftIO $ putStrLn ("h = " ++ show h)
    liftIO $ putStrLn (show hs)

dumpCell i = let
        sepStr' [] _ = ""
        sepStr' [x] _ = x
        sepStr' (x:xs) del = x ++ del ++ (sepStr' xs del)
    in do
    v <- deref i
    case v of
       (Var i) -> return ("V"++show i)
       (Str i) -> do
            (Struct (funct,arity)) <- get_cell i
            cs <- get_cells (i+1) arity
            ss <- mapM dumpCell cs
            return (funct ++ "(" ++ sepStr' ss "," ++ ")")
       (Cons a) -> return a
       _ -> error ("cannot print " ++ show v)


traceCommand i = do
    liftIO $ putStr (wamEmitInstr i)
    let (_,rs) = i
    rs' <- mapM (\i -> get_content i >>= dumpCell) rs
    liftIO $ putStrLn (show rs')


hasChoicePoint = do
    b <- gets reg_b
    return (b > 1000)


evalWam m = evalStateT (runContT m (\a -> return ())) st 
   where st = WamState { idx  = undefined
                       , mem  = undefined
                       , code = undefined
                       , regs = undefined
                       , reg_p = 0
                       , reg_c = 0
                       , reg_b = 0
                       , reg_s = 0
                       , reg_a = 0
                       , reg_t = 0
                       , reg_h = 0
                       , reg_e = 0
                      }

wamExecute :: WamProgram            -- ^ the compiled wam program
           -> WamRuntime [WamCell]  -- ^ a list of wamcells containing the goal variables
wamExecute p =
    let index = wamIndex p
        instr = wamCode p

        ((g_name, g_arity), g_addr) = 
               case filter (\((x,i),_) -> x == "?") index of
                          []  -> error "goal not found"
                          [x] -> x
        init = do
            init_prog
            init_mem (2^20) 100

        init_prog = do
            c <- liftIO $ newListArray (1, 1024) instr
            modify (\st -> st{ code  = c
                             , idx   = index
                             , reg_p = 0
                             , reg_c = 0
                             , reg_s = 0
                             })

        init_mem arraysize regnum = do
            m <- liftIO $ newArray_ (1, arraysize) :: WamRuntime (IOArray WamAddress WamCell)
            r <- liftIO $ newArray_ (1, regnum)    :: WamRuntime (IOArray Int WamCell)
            modify (\st -> st{ mem   = m
                             , regs  = r
                             , reg_e = startAndStack
                             , reg_b = startOrStack
                             , reg_h = startHeap
                             , reg_t = startTrail
                             })
          where startHeap     = 0
                startAndStack = startHeap + 1000
                startOrStack  = startAndStack
                startTrail    = startAndStack + 1000

        init_goal = do 
            set_arity g_arity
            mapM_ (\i -> create_unbound i >> restore_arg i i) [1..g_arity]
            h <- gets reg_h
            modify (\s -> s{reg_h = h + g_arity})
            jump g_addr

        loop = do
            step
            p <- gets reg_p
            when (not(p == 0)) loop -- (p <= m) loop

        run = do
            loop
    in init >> init_goal >> run >> get_cells 1 g_arity


step = do
    i <- gets reg_p >>= readinstr
    advance 1
    sem i
    traceCommand i

deref a =
    case a of
       Var x -> do
           c <- get_cell x
           if not (unBound x c) 
           then deref c
           else return a
       _ -> return a
    where unBound x (Var y) = (x == y)
          unBound _ _ = False


allocate :: Int -> WamRuntime ()
allocate n = do
    b <- gets reg_b
    e <- gets reg_e
    let k = max b e
    c <- gets reg_c
    e <- gets reg_e
    mapM_ create_unbound [k+1..k+n]
    change_cell (k+n+1) (Addr c)
    change_cell (k+n+2) (Addr e)
    modify (\s -> s{reg_e = k + n + 2})

deallocate :: WamRuntime ()
deallocate = do
    e  <- gets reg_e
    let k = e
    --(Addr k)  <- get_cell e
    (Addr e') <- get_cell k
    (Addr c') <- get_cell (k-1)
    modify (\s -> s{reg_e = e', reg_c = c'})


procedure_address q = do
    preds <- gets idx
    return $ lookup q preds

execute (q,n) = do
    set_arity n
    r <- procedure_address (q,n)
    case r of
        Just a -> jump a
        Nothing -> backtrack

call q = set_return_address >> execute q

execute_variable r n =
    let execute' x n = do
            dx <- deref x
            case dx of
                App a m -> do
                    app <- get_cell a
                    mapM_ (\i -> restore_arg (a+m+1-i) (n+i)) [1..m]
                    execute' app (n+m)
                Struct str -> 
                    execute str
                Var v -> 
                    error "Uninstantiated higher order variable"
    in do
        x <- get_content r
        execute' x n

call_variable x n = set_return_address >> execute_variable x n

bind' (Temp i) x = set_temp i x
bind' (Perm i) x = get_perm_real_addr i >>= \a -> bind a x

bind a x =
    let
        trail a = do
            b <- gets reg_b
            hasChoice <- hasChoicePoint
            when hasChoice $ do
                (Addr ss) <- get_cell (b-2)
                when (a < ss || a <= b) $ do
                    t <- gets reg_t
                    modify (\st -> st{reg_t = t + 1})
                    change_cell (t + 1) (Addr a)
    in do
       trail a
       change_cell a x

stabilize' u v = do
    h <- gets reg_h
    create_unbound (h+1)
    modify (\st -> st{reg_h = h+1})
    c <- get_cell (h+1)
    bind' u c
    bind' v c

stabilize a b = do
    h <- gets reg_h
    create_unbound (h+1)
    modify (\st -> st{reg_h = h+1})
    c <- get_cell (h+1)
    bind' a c
    bind b c

precedes (Var x) (Var y) = x < y
precedes _ _ = error "cell content not variables"

-- occur check not implemented
occursin x y = False

get_cells start count = mapM get_cell [start..(start+count-1)]

unify_lists [] _ = return ()
unify_lists (x:xs) (y:ys) = let
        isVar (Var _) = True
        isVar _ = False
        unify' x@(Var a) y@(Var b) = do
            if x `precedes` y 
            then bind b x
            else bind a y
            unify_lists xs ys
        unify' x@(Var a) y = if x `occursin` y then backtrack else bind a y >> unify_lists xs ys
        unify' x y@(Var b) = if y `occursin` x then backtrack else bind b x >> unify_lists xs ys
        unify' (Str a) (Str b) = do
            Struct (f,n) <- get_cell a
            Struct (g,m) <- get_cell b
            if (f == g && n == m) 
            then do
                fs <- get_cells (a+1) n
                gs <- get_cells (b+1) m
                unify_lists (fs ++ xs) (gs ++ ys)
            else
                backtrack
        unify' _ _ = backtrack
     in do
        x' <- deref x
        y' <- deref y
        if (x' == y')
        then unify_lists xs ys
        else unify' x' y'

unify x y = unify_lists [x] [y]

get_value x z = do
    x' <- get_content x
    z' <- get_content z
    unify x' z'

get_variable z x = get_content x >>= bind' z

get_constant c z = get_content z >>= unify c

push_structure (f,n) = do
    h <- gets reg_h
    change_cell (h+1) (Str (h+2))
    change_cell (h+2) (Struct (f,n))
    mapM_ create_unbound [(h+3)..(h+n+2)]
    modify (\st -> st{ reg_h = h + n + 2
                     , reg_s = h + 3
                     })

get_structure str x = do
    x' <- get_content x
    dx <- deref x'
    case dx of
        Var _ -> do
            h <- gets reg_h
            push_structure str
            hc <- get_cell (h+1)
            unify dx hc
        Str a -> do
            Struct str' <- get_cell a
            if str == str'
            then set_structure_pointer a >> next_arg 
            else backtrack
        _ -> backtrack


-- put_variable z x = get_content x >>= \x' -> bind' z x'
-- put_variable z@(Temp i) x = do
--    h <- gets reg_h
--    create_unbound (h+1)
--    c <- get_cell (h+1)
--    modify (\st -> st{reg_h = h + 1})
--    set_content z c
--    set_content x c
    -- set_temp z

put_variable z@(Temp i) x = stabilize' z x

put_variable z@(Perm i) x = do
    a <- get_perm_real_addr i
    create_unbound a
    a' <- get_cell a
    set_content z a'
    --set_content x a'
    bind' x a'

is_unstable (Var r) = do
    e <- gets reg_e
    (Addr e') <- get_cell e
    return (e' < r)

is_unstable _ = return False

put_unsafe_value y x = do
    yc <- get_content y
    dy <- deref yc
    u <- is_unstable dy

    if u then
        let (Var r) = dy in stabilize x r
    else
        bind' x dy

put_value z x = do
    content <- get_content z
    bind' x content

put_constant c z = bind' z c

put_structure f z = do
    h <- gets reg_h
    push_structure f
    hc <- get_cell (h+1)
    bind' z hc

unify_variable z = do
    s <- gets reg_s
    b <- get_cell s
    bind' z b
    next_arg

unify_value z = do
    a <- get_content z
    s <- gets reg_s
    b <- get_cell s
    next_arg
    unify a b

unify_constant c = do
    s <- gets reg_s
    content <- get_cell s
    next_arg
    unify c content

try_me_else p = do
    b <- gets reg_b
    e <- gets reg_e
    a <- gets reg_a
    let k = max b e
    mapM_ (\i -> save_arg (k+i) i) [1..a]
    let ka = k + a
    save_reg (ka+1) reg_a
    save_reg (ka+2) reg_e
    save_reg (ka+3) reg_t
    save_reg (ka+4) reg_c
    save_reg (ka+5) reg_h
    save_reg (ka+6) reg_b
    change_cell (ka+7) (Addr p)
    modify (\s -> s{reg_b = ka + 7})

retry_me_else p = do
    b <- gets reg_b
    change_cell b (Addr p)

trust_me_else_fail = do
    b <- gets reg_b
    (Addr b') <- get_cell (b-1)
    modify (\s->s{reg_b = b'})

unwind t = do
    t' <- gets reg_t
    when (t' > t) $ do
        ts <- mapM get_cell [t+1..t']
        mapM_ (create_unbound) (map (\(Addr a)->a) ts)
        modify (\s -> s{reg_t = t})

backtrack :: WamRuntime ()
backtrack = do
    k <- gets reg_b
    (Addr p) <- get_cell k
    (Addr b) <- get_cell (k-1)
    (Addr h) <- get_cell (k-2)
    (Addr c) <- get_cell (k-3)
    (Addr t) <- get_cell (k-4)
    (Addr e) <- get_cell (k-5)
    (Addr a) <- get_cell (k-6)
    mapM_ (\i -> restore_arg (k-7-a+i) i) [1..a]
    unwind t
    modify (\s -> s{ reg_p = p
                   , reg_h = h
                   , reg_c = c
                   , reg_e = e
                   , reg_a = a
                   })

-- | sem executes the operational semantics of the wam instruction
sem :: WamInstr         -- ^ the wam instruction
    -> WamRuntime () 
sem (Allocate n, _)         = allocate n
sem (Deallocate, _)         = deallocate
sem (Proceed, _)            = proceed
sem (Call s, _)             = call s
sem (Execute s, _)          = execute s
sem (TryMeElse n, _)        = try_me_else n
sem (RetryMeElse n, _)      = retry_me_else n
sem (TrustMe, _)            = trust_me_else_fail
sem (Backtrack, _)          = backtrack
sem (GetConstant s, [z])    = get_constant (Cons s) z
sem (GetVariable, [x,z])    = get_variable x z
sem (GetValue, [x,z])       = get_value x z
sem (GetStructure f, [x])   = get_structure f x
sem (PutValue, [x,z])       = put_value x z
sem (PutUnsafeValue, [x,z]) = put_unsafe_value x z
sem (PutVariable, [x,z])    = put_variable x z
sem (PutStructure f, [x])   = put_structure f x
sem (PutConstant c, [x])    = put_constant (Cons c) x
sem (UnifyConstant c,_)     = unify_constant (Cons c)
sem (UnifyValue, [x])       = unify_value x
sem (UnifyVariable, [x])    = unify_variable x
sem x                       = error $ "unknown instruction " ++ show x

