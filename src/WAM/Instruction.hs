module WAM.Instruction where


import Prolog (VarId)

type WamAddress = Int

-- WAM Operator
data WamOp =
     PutVariable
   | PutValue
   | PutUnsafeValue
   | PutApplication Int
   | PutStructure WamLabel
   | PutConstant String
   | GetStructure WamLabel
   | GetConstant String
   | GetValue
   | GetVariable
   | UnifyConstant String
   | UnifyValue
   | UnifyVariable
   | Call WamLabel
   | Execute WamLabel
   | Proceed
   | Allocate Int
   | Deallocate
   | TryMeElse WamAddress
   | RetryMeElse WamAddress
   | TrustMe
   | Backtrack
   | CallVariable
   | ExecuteVariable
   deriving Show

type WamArg = WamRegister

-- WAM Instruction
type WamInstr = (WamOp, [WamArg])

getOp  (op, _)   = op
getReg (_, regs) = regs

data WamRegister = 
    Perm Int 
  | Temp Int 
  deriving Show

type WamInstrSeq = [WamInstr]

type WamLabel = (String, Int)

type WamIndex = [(WamLabel, WamAddress)]

type WamGoal = ([VarId], WamInstrSeq)


-- WAM Program
data WamProgram = 
    DB { wamIndex    :: WamIndex 
       , wamCode     :: WamInstrSeq
       }


mkDB :: [(WamLabel, WamInstrSeq)] -> WamProgram
mkDB lst = DB { wamIndex = idx, wamCode = code }
    where 
        (idx, code) = foldl aux ([],[]) lst
        aux (idx, code) (lbl, instr) = (idx ++ [(lbl, length code + 1)], code ++ instr)

