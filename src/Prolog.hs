-----------------------------------------------------------------------------
--
-- Module      :  Main
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

data Term    =  T (String, [Term]) | V String deriving Show
type Clause  = (Term, [Term])
type Goal = [Term]
type Program = (Goal,[Clause])

varsTerm t = nub $ vars' t
    where vars' (T (_, ts)) = concatMap vars' ts
          vars' (V v) = [v]

varsClause (t, ts) = nub $ varsTerm t ++ concatMap varsTerm ts

varsGoal ts = nub $ concatMap varsTerm ts

preds cs =  nub $ map (\(T (s,args),_) -> (s, length args)) cs

defs cs p =  filter isOfPred cs where isOfPred (T (s,_),_) = s == p

-- Simple Prolog Parse Combinators

schar c = char c >> spaces

atom = lower >>= \x -> many alphaNum >>= \xs -> return (x:xs)

variable = upper >>= \x -> many alphaNum >>= \xs -> return (V (x:xs))

struct = atom >>= \a -> arguments >>= \ts -> return (T (a, ts))

list = schar '[' >> terms >>= \ts -> listTail >>= \t -> return $ makeLst ts t
   where makeLst [] cdr = cdr
         makeLst (x:xs) cdr = T ("cons", [x, (makeLst xs cdr)])

listTail = (schar '|' >> term >>= \t -> schar ']' >> return t) <|> (schar ']' >> return (T ("nil",[])))

arguments = ((schar '(' >> terms >>= \ls -> schar ')' >> return ls)) <|>
            (spaces >> return [])

term = variable <|> struct <|> list

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

prolog = goal >>= \g ->  clauses >>= \cs -> return (g, cs)

parseprolog = parse prolog
