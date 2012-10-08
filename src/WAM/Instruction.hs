module WAM.Instruction where


type WamAddress = Int

-- WAM Operator
data WamOp =
     PutVariable
   | PutValue
   | PutUnsafeValue
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

data WamRegister = Perm Int | Temp Int  deriving Show

-- WAM Program
data WamProgram = P [String] [(WamLabel,WamAddress)] [WamInstr]


type WamLabel = (String, Int)

type WamIndex = [(WamLabel, WamAddress)]
