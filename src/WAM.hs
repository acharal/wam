
module WAM where

import Debug.Trace

import Data.List (nub)

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
   deriving Show

type WamArg = WamRegister

-- WAM Instruction
type WamInstr = (WamOp, [WamArg])

data WamRegister = Perm Int | Temp Int  deriving Show

-- WAM Program
data WamProgram = P [String] [((String,Int),WamAddress)] [WamInstr]


-- dump

dumpWamCommand (Allocate n, []) = "allocate " ++ show n
dumpWamCommand (Deallocate, []) = "deallocate"
dumpWamCommand (Execute (p,n), []) = "execute " ++ p ++ "/" ++ show n
dumpWamCommand (Call (p,n), []) = "call " ++ p ++ "/" ++ show n
dumpWamCommand (Proceed, []) = "proceed"
dumpWamCommand (GetStructure (s,n), xs) = "get_structure " ++ s ++ "/" ++ show n ++ ", " ++ dumpRegs xs
dumpWamCommand (GetConstant s, xs) = "get_constant " ++ s ++ ", " ++ dumpRegs xs
dumpWamCommand (PutStructure (s,n), xs) = "put_structure " ++ s ++ "/" ++ show n ++ ", " ++ dumpRegs xs
dumpWamCommand (PutConstant s, xs) = "put_constant " ++ s ++ ", " ++ dumpRegs xs
dumpWamCommand (TryMeElse n, _) = "try_me_else " ++ show n
dumpWamCommand (RetryMeElse n, _) = "retry_me_else " ++ show n
dumpWamCommand (TrustMe, _) = "trust_me_or_fail "
dumpWamCommand (PutVariable, xs) = "put_variable "++ dumpRegs xs
dumpWamCommand (PutValue, xs) = "put_value "++ dumpRegs xs
dumpWamCommand (PutUnsafeValue, xs) = "put_unsafe_value "++ dumpRegs xs
dumpWamCommand (GetVariable, xs) = "get_variable " ++ dumpRegs xs
dumpWamCommand (GetValue, xs) = "get_value " ++ dumpRegs xs
dumpWamCommand (UnifyVariable, xs) = "unify_variable " ++ dumpRegs xs
dumpWamCommand (UnifyValue, xs) = "unify_value " ++ dumpRegs xs
dumpWamCommand (UnifyConstant s, xs) = "unify_constant " ++ s
dumpRegister (Temp i) = "X"++(show i)
dumpRegister (Perm i) = "Y"++(show i)
dumpRegs xs = a $ map dumpRegister xs
     where a [] = ""
           a [s] = s
           a (s:ss) = s ++ ", " ++ (a ss)

dumpIndex [] = ""
dumpIndex (((s,n),i):idx) = "(" ++ s ++ "/" ++ show n ++ ", " ++ show i ++ ")\n" ++ dumpIndex idx

dumpWAMProgram (P _ idx ws) = concatMap (\w -> dumpWamCommand w ++ "\n") ws ++ dumpIndex idx



