-----------------------------------------------------------------------------
--
-- Module      :  Prolog
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

module Prolog where

import Text.ParserCombinators.Parsec
import Data.List (nub)

type VarId = String -- deriving (Eq, Show)

data Term    =  
    T (String, [Term]) 
  | V VarId
  | V2 (VarId, [Term]) 
  deriving (Show, Eq)

type Clause  = (Term, [Term])

type Goal    = [Term]

type Program = (Goal,[Clause])

data Type = TypeO | TypeI | F Type Type deriving Show

varsTerm t = nub $ vars' t
    where vars' (T (_, ts)) = concatMap vars' ts
          vars' (V v) = [v]

varsClause (t, ts) = nub $ varsTerm t ++ concatMap varsTerm ts

varsGoal ts = nub $ concatMap varsTerm ts

preds cs =  nub $ map (\(T (s,args),_) -> (s, length args)) cs

defs cs (p,n) =  filter isOfPred cs 
    where isOfPred (T (s,args),_) = s == p && length args == n

args (T (_, x))    = x
functor (T (x, _)) = x

-- types

order (TypeO) = 0
order (TypeI) = 0
order (F t1 t2) = max (1 + (order t1)) (order t2)

typesVarsClause (t, ts) tysig = typesVars t TypeO tysig

-- typesVars :: -> Type ->  [(String, Type)]
typesVars (T (s, args)) ty tysig =
    let argTy (F t1 t2) = t1:(argTy t2)
        argTy _ = []
    in case lookup s tysig of
        Just funty -> concatMap (\(a,t) -> typesVars a t tysig) $ zip args (argTy funty)
        Nothing -> []-- error $ "cannot find type for " ++ s
typesVars (V v) ty tysig = [(v,ty)]

-- Simple Prolog Parse Combinators

schar c = char c >> spaces

atom = lower >>= \x -> many alphaNum >>= \xs -> return (x:xs)

variable = upper >>= \x -> many alphaNum >>= \xs -> return (V (x:xs))

struct = (atom >>= \a -> arguments >>= \ts -> return (T (a, ts))) <|>
         (variable >>= \(V s) -> arguments >>= \ts -> return (if ts == [] then (V s) else (V2 (s, ts))))

list = schar '[' >> terms >>= \ts -> listTail >>= \t -> return $ makeLst ts t
   where makeLst [] cdr = cdr
         makeLst (x:xs) cdr = T ("cons", [x, (makeLst xs cdr)])

listTail = (schar '|' >> term >>= \t -> schar ']' >> return t) <|> (schar ']' >> return (T ("nil",[])))

arguments = ((schar '(' >> terms >>= \ls -> schar ')' >> return ls)) <|>
            (spaces >> return [])

term = struct <|> variable <|> list

terms :: Parser [Term]
terms = sepBy term (schar ',')

goal = spaces >> string "?-" >> spaces >> terms >>= \t -> schar '.' >> return t

clause = do
    spaces
    head <- term
    spaces
    body <- (string ":-" >> spaces >> terms) <|> (spaces >> return [])
    (char '.') >> spaces
    return (head, body)

clauses = many clause

comment :: Parser ()
comment = (char '%' >> skipMany anyToken)

typ = typ1 `chainr1` (do { spaces; string "->"; spaces; return F})
    where typ1 =  grtype <|> (schar '(' >> spaces >> typ >>= \t -> schar ')' >> return t)
          grtype = typo <|> typi
          typo = string "o" >> return TypeO
          typi = string "i" >> return TypeI


typsig = do
    a <- atom
    spaces
    string "::"
    spaces
    t <- typ
    schar '.'
    return (a,t)


prolog = do 
    g <- goal 
    cs <- clauses
    return (g, cs)

hoprolog = do
    ts <- many typsig
    spaces
    prog <- prolog
    return (ts, prog)

parseprolog   = parse prolog
parsehoprolog = parse hoprolog
