module WAM.Runtime.Trace where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Trans

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

-- type WamTraceT = TraceT WamInstr

trace i = TraceT (yield i)

runTraceT m = pogoStick traceInstr (unTrace m)
    where traceInstr (Yield i c) =  traceCommand i >> c
--    where traceInstr (Yield _ c) = c

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

{- p : command : { register values }  -}

traceCommand i = do
    liftIO $ putStr (render $ pprInstr i)
    liftIO $ putStr "\n"
--    let (_,rs) = i
--    rs' <- mapM (\i -> get_content i >>= dumpCell) rs
--    liftIO $ putStrLn (show rs')


