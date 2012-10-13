module WAM.Runtime.Trace where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State

import WAM.Instruction
import WAM.Emit
import WAM.Runtime.Mem

import Text.PrettyPrint hiding (Str)

type TraceT s = Coroutine (Yield s)

type WamTraceT = TraceT WamInstr

trace = yield

runTraceT m = pogoStick traceInstr m
    where traceInstr (Yield i c) =  traceCommand i >> c
--    where traceInstr (Yield _ c) = c

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
            return (text "_X" <> int i)
       Str i -> do
            Struct (funct, arity) <- get_cell i
            cs <- get_cells (i+1) arity
            ss <- mapM pprCell cs
            return $ text funct <> parens (sep $ punctuate comma ss)
       Cons a -> 
            return (text a)
       _ ->
            error ("cannot print " ++ show v)

traceCommand i = do
    liftIO $ putStr (wamEmitInstr i)
    liftIO $ putStr "\n"
--    let (_,rs) = i
--    rs' <- mapM (\i -> get_content i >>= dumpCell) rs
--    liftIO $ putStrLn (show rs')


