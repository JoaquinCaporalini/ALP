module Ejercicio04 where

-- Solucion por Juan Manuel --

import Parsing

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Expr = Num Int | BinOp Op Expr Expr
data Op = Add | Mul | Min | Div

-- expr -> term ('+' expr | '-' expr | e)
expr :: Parser Expr
expr =  do 
          t <- term
          ((do char '+'
               e <- expr
               return (BinOp Add t e)
           <|> 
           do char '-'
              e <- expr
              return (BinOp Min t e)
           )
           <|>
           return t)

-- term -> factor ('*' term | '/' term | e)
term :: Parser Expr
term = do f <- factor
          ((do char '*'
               t <- term
               return (BinOp Mul f t)
           <|>
           do char '/'
              t <- term
              return (BinOp Div f t))
           <|>
           return f)

--factor -> digit | '(' expr ')'
-- digit -> '0' | '1' | ... | '9'
factor :: Parser Expr
factor = do d <- digit
            return (Num (digitToInt d))
            -- return (parse integer d)
         <|> 
         do char '('
            e <- expr
            char ')'
            return e

eval :: String -> Expr
eval xs = fst (head (parse expr xs))