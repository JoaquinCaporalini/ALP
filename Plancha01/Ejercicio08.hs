module Ejercicio08 where

-- Solucion por Valentina --

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
          e' <- expr'
          return (e' t)

expr' :: Parser (Expr -> Expr)
expr' =  (do char '+'
             t <- term
             e' <- expr'
             return (e' . (\e -> (BinOp Add e t))))
         <|> 
         (do char '-'
             t <- term
             e' <- expr'
             return (e' . (\e -> (BinOp Min t e))))
         <|>
         return id

-- term -> factor ('*' term | '/' term | e)
term :: Parser Expr
term = do f <- factor
          f'<- term'
          return (f' f)
          

term' :: Parser (Expr -> Expr)
term' = ((do char '*'
             t <- term
             t' <- term'
             return (t' . (\e -> (BinOp Mul t e))))
             -- return (BinOp Mul f t)
        <|>
        (do char '/'
            t <- term
            t' <- term'
            return (t' . (\e -> (BinOp Div t e))))
            -- return (BinOp Div f t))
        <|>
        return id)

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