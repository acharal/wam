module WAM.Instruction where


type WamAddress = Int

-- WAM Operator
data WamOp =
     PutVariable
   | PutValue
   | PutUnsafeValue
   | PutStructure (String,Int)
   | PutConstant String
   | GetStructure (String,Int)
   | GetConstant String
   | GetValue
   | GetVariable
   | UnifyConstant String
   | UnifyValue
   | UnifyVariable
   | Call (String,Int)
   | Execute (String,Int)
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

data WamRegister = Perm Int | Temp Int  deriving Show

-- WAM Program
data WamProgram = P [String] [((String,Int),WamAddress)] [WamInstr]



