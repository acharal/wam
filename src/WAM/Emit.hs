module WAM.Emit (wamEmitInstr, wamEmitProg) where

import WAM.Instruction
import Text.PrettyPrint


-- aux

pprInstrAux name args = text name <> parens (sep $ punctuate comma args)
pprPredAux predname arity = text predname <> char '/' <> int arity

pprRegs xs = map pprReg xs
pprReg (Temp i) = text "x" <> parens (int i)
pprReg (Perm i) = text "y" <> parens (int i)


-- dump

pprInstr (Allocate n, _)          = pprInstrAux "allocate" [int n]
pprInstr (Deallocate, _)          = pprInstrAux "deallocate" []
pprInstr (Execute (p,n), _)       = pprInstrAux "execute" [pprPredAux p n]
pprInstr (Call (p,n), _)          = pprInstrAux "call" [pprPredAux p n]
pprInstr (Proceed, [])            = pprInstrAux "proceed" []
pprInstr (GetStructure (s,n), xs) = pprInstrAux "get_structure" ((pprPredAux s n):pprRegs xs)
pprInstr (PutStructure (s,n), xs) = pprInstrAux "put_structure" ((pprPredAux s n):pprRegs xs)
pprInstr (GetConstant  s, xs)     = pprInstrAux "get_constant" ((text s):pprRegs xs)
pprInstr (PutConstant  s, xs)     = pprInstrAux "put_constant" ((text s):pprRegs xs)
pprInstr (UnifyConstant  s, xs)   = pprInstrAux "unify_constant" ((text s):pprRegs xs)
pprInstr (GetVariable, xs)        = pprInstrAux "get_variable" (pprRegs xs)
pprInstr (PutVariable, xs)        = pprInstrAux "put_variable" (pprRegs xs)
pprInstr (UnifyVariable, xs)      = pprInstrAux "unify_variable" (pprRegs xs)
pprInstr (GetValue, xs)           = pprInstrAux "get_value"    (pprRegs xs)
pprInstr (PutValue, xs)           = pprInstrAux "put_value"    (pprRegs xs)
pprInstr (PutUnsafeValue, xs)     = pprInstrAux "put_unsafe_value"  (pprRegs xs)
pprInstr (UnifyValue, xs)         = pprInstrAux "unify_value"    (pprRegs xs)

pprInstr (TryMeElse n, _)         = pprInstrAux "try_me_else" [int n]
pprInstr (RetryMeElse n, _)       = pprInstrAux "retry_me_else" [int n]
pprInstr (TrustMe, _)             = pprInstrAux "trust_me_or_fail" []


pprIdx idx = vcat $ map pprIdxEntry idx
    where pprIdxEntry ((s,n),i) = parens $ sep $ punctuate comma [ pprPredAux s n, int i ]

pprProg (P _ idx ws) = (vcat $ map pprInstr ws) $$ (pprIdx idx)

wamEmitInstr i = render $ pprInstr i
wamEmitProg p  = render $ pprProg p

dumpIndex [] = ""
dumpIndex (((s,n),i):idx) = "(" ++ s ++ "/" ++ show n ++ ", " ++ show i ++ ")\n" ++ dumpIndex idx

