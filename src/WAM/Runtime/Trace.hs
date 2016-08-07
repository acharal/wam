module WAM.Runtime.Trace where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative (Applicative(..))
import WAM.Instruction
import WAM.Emit
import WAM.Runtime.Mem

import Text.PrettyPrint hiding (Str)

{-
 - trace before and after the execution of an instruction
 - show content of registers involved in an instruction
 - pause execution waiting for user to resume
 - show stack and content of registers
-}

newtype TraceT s m a = TraceT { unTrace :: Coroutine (Yield s) m a }


class Monad m => MonadTrace a m where
	trace :: a -> m ()

instance Monad m => MonadTrace a (TraceT a m) where
	trace a = TraceT (yield a)

-- type WamTraceT = TraceT WamInstr

{-
trace' i = TraceT (yield i)

trace m = do 
    p <- gets reg_p
    i <- get_instr p
    a <- m
    trace' (p,i)
    return a
-}

runTraceT t m = pogoStick traceInstr (unTrace m)
    where traceInstr (Yield i c) =  t i >> c
--    where traceInstr (Yield _ c) = c
instance Monad m => Functor (TraceT s' m) where
    fmap = liftM

instance Monad m => Applicative (TraceT s' m) where
    pure a = TraceT $ return a
    (<*>) = ap

instance Monad m => Monad (TraceT s' m) where
    return  = TraceT . return
    a >>= b = TraceT (unTrace a >>= \x -> unTrace (b x))

instance MonadIO m => MonadIO (TraceT s' m) where
    liftIO = lift . liftIO 

instance MonadTrans (TraceT s') where
    lift = TraceT . lift

instance MonadState s m => MonadState s (TraceT s' m) where
    get = lift get
    put = lift . put

instance (Functor s', MonadState s m) => MonadState s (Coroutine s' m) where
    get = lift get
    put = lift . put

-- trace 

deref' a =
    case a of
       Var x -> do
           c <- get_cell x
           if not (unBound x c) 
           then deref' c
           else return a
       _ -> return a
    where unBound x (Var y) = (x == y)
          unBound _ _ = False

{- 
dumpState = do
    h  <- gets reg_h
    m  <- gets mem
    hs <- get_cells 1 h
    liftIO $ putStrLn ("h = " ++ show h)
    liftIO $ putStrLn (show hs)
-}

dumpCell i = do 
    p <- pprCell i
    return $ render p

pprCell i = do
    v <- deref' i
    case v of
       Var i -> 
            return (text "X" <> int i)
       Str i -> do
            Struct (funct, arity) <- get_cell i
            cs <- get_cells (i+1) arity
            ss <- mapM pprCell cs
            return $ text funct <> parens (sep $ punctuate comma ss)
       Cons a -> 
            return (text a)
       _ ->
            error ("cannot print " ++ show v)

trim n p =
    let r = render p
    in if length r <= n
       then p
       else text $ take (n-3) r ++ "..."
        

traceRegs regs = do
    regReps <- mapM (\(r, c) -> pprCell c  >>= \c' -> return (pprReg r, c')) regs
    return $ brackets (sep $ punctuate comma $ map (\(r,c)-> r <+> text "=" <+> trim 20 c) regReps)

{- p : command : { register values }  -}

traceCommand (p,i) = do
    cont <- mapM get_content (getReg i)
    regs <- traceRegs $ zip (getReg i) cont
    liftIO $ putStr (render $ (int p) $$ nest 5 (pprInstr i) $$ nest 35 regs)
    liftIO $ putStr "\n"
--    let (_,rs) = i
--    rs' <- mapM (\i -> get_content i >>= dumpCell) rs
--    liftIO $ putStrLn (show rs')


