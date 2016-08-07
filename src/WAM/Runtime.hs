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
     wamExecute
   , evalWam
   ) where

import Prolog (VarId)
import WAM
import WAM.Runtime.Mem
import WAM.Runtime.Trace (MonadTrace, trace)


import Control.Monad
import Control.Monad.State
import Control.Applicative

-- type WamRuntime = StateT WamState (ContT () IO)
newtype WamRuntime c i a = WamRuntime { unWam :: (StateT (WamState c i) IO a) }

instance Functor (WamRuntime c i) where
        fmap f a = WamRuntime $ liftM f (unWam a)

instance Applicative (WamRuntime c i) where
        pure a = WamRuntime $ return a 
        (<*>)  = ap

instance Monad (WamRuntime c i) where
	return s = WamRuntime $ return s
	m >>= f =  WamRuntime (unWam m >>= \s -> unWam (f s))

instance MonadState (WamState c i) (WamRuntime c i) where
	get = WamRuntime $ get
	put s = WamRuntime $ put s

instance MonadIO (WamRuntime c i) where
	liftIO io = WamRuntime (liftIO io)

instance MonadTrace a (WamRuntime c i) where
	trace a = return ()

type WamResult c = [(VarId, c)]


-- | make cell i unbound, namely a variable cell with self-reference
create_unbound i = change_cell i (Var i)

-- code control

-- | advance the program register by n
advance n = gets reg_p >>= \p ->  jump (p + n)

-- | jump to a specific address
-- jump :: WamAddress -> WamRuntime ()
jump p' = modify (\s -> s{reg_p = p'})

-- | read the wam instruction from the current location of p
readinstr n = get_instr n

-- set_return_address :: WamRuntime ()
set_return_address = gets reg_p >>= \p -> modify (\s -> s{reg_c = p})

-- set_structure_pointer :: WamAddress -> WamRuntime ()
set_structure_pointer s = modify (\st -> st{reg_s = s})

-- proceed :: WamRuntime ()
proceed = gets reg_c >>= \c -> jump c

-- register management



save_arg i a = get_temp a >>= change_cell i
restore_arg i a = get_cell i >>= set_temp a


-- arity management 

set_arity n = modify (\s -> s{reg_a = n})

-- next_arg :: WamRuntime ()
next_arg = gets reg_s >>= \s -> modify (\st -> st{reg_s = s + 1})


-- error 

unexpected msg = error msg

hasChoicePoint = do
    b <- gets reg_b
    return (b > 1000)

evalWam m = evalStateT (unWam m) emptyWamState

{-
wamExecute :: WamProgram            -- ^ the compiled wam program
           -> WamGoal               -- ^ the compiled goal
           -> WamRuntime WamResult  -- ^ a list of wamcells containing the goal variables
-}
wamExecute p g =
    let index = wamIndex p
        instr = wamCode p

        (vars, goal) = g
        g_arity      = length vars
        g_addr       = length instr + 1

        init = do
            init_prog (instr ++ goal)
            init_mem (2^20) 100

        init_prog i = do
            init_code i 
            modify (\st -> st{ idx   = index
                             , reg_p = 0
                             , reg_c = 0
                             , reg_s = 0
                             })
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
    in do 
        init
        init_goal
        run
        cells <- get_cells 1 g_arity
        return $ zip vars cells

withTrace m = do 
    p <- gets reg_p
    i <- get_instr p
    a <- m
    trace (p,i)
    return a

step = withTrace $ do
    i <- gets reg_p >>= readinstr
    advance 1
    sem i

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
        Just a  -> jump a
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
                    unexpected "Uninstantiated higher order variable"
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
precedes _ _ = unexpected "cell content not variables"

-- occur check not implemented
occursin x y = False

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

-- backtrack :: WamRuntime ()
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
{-
sem :: WamInstr         -- ^ the wam instruction
    -> WamRuntime () 
-}
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
sem x                       = unexpected $ "unknown instruction " ++ show x

