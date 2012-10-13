module WAM.Runtime.Mem where


import WAM.Instruction
import Data.Array.IO (IOArray, readArray, writeArray, newArray_, newListArray)
import Control.Monad.State

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


data WamState = WamState { idx   :: WamIndex      -- ^ predicate index
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


emptyWamState = 
    WamState { idx  = undefined
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

init_mem arraysize regnum = do
     m <- liftIO $ newArray_ (1, arraysize)
     r <- liftIO $ newArray_ (1, regnum)
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

init_code i = do
      c <- liftIO $ newListArray (1, 1024) i
      modify (\st -> st{ code  = c
                       })


-- memory management

-- | changes the content of the cell in the global memory space
change_cell i c = do
    st <- gets mem
    liftIO $ writeArray st i c

get_cell i = do
    st <- gets mem
    liftIO $ readArray st i

get_instr n = do
    st <- gets code
    liftIO $ readArray st n

get_cells start count = mapM get_cell [start..(start+count-1)]

-- | saves the content of a register in the cell in memory
save_reg i r = do
    a <- gets r
    change_cell i (Addr a)

get_temp i = do
    rs <- gets regs
    liftIO $ readArray rs i

set_temp i c = do
    rs <- gets regs
    liftIO $ writeArray rs i c

