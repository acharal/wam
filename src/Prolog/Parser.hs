module Prolog.Parser where

import Prolog
import Text.ParserCombinators.Parsec

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
