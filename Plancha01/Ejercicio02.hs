module Ejercicio02 where

import Parsing

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

-- expr -> term ('+' expr | '-' expr | e)
expr :: Parser Int
expr =  do 
          t <- term
          ((do char '+'
               e <- expr
               return (t + e)
           <|> 
           do char '-'
              e <- expr
              return (t - e)
           )
           <|>
           return t)

-- term -> factor ('*' term | '/' term | e)
term :: Parser Int
term = do f <- factor
          ((do char '*'
               t <- term
               return (f * t)
           <|>
           do char '/'
              t <- term
              return (f * t))
           <|>
           return f )
--factor -> digit | '(' expr ')'
-- digit -> '0' | '1' | ... | '9'
factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
            -- return (parse integer d)
         <|> 
         do char '('
            e <- expr
            char ')'
            return e

eval :: String -> Int
eval xs = fst (head (parse expr xs))


-- module Ejercicio02 where

-- import Parsing

-- import Data.Char
-- import Control.Monad
-- import Control.Applicative hiding (many)

-- expr :: Parser Int
-- expr =  do 
--           t <- term
--           (do char '+'
--               e <- expr
--               return (t + e)
--            <|> 
--            return t)

-- term :: Parser Int
-- term = do f <- factor
--           (do char '*'
--               t <- term
--               return (f * t)
--            <|> 
--            return f )

-- factor :: Parser Int
-- factor = do d <- digit
--             return (digitToInt d)
--          <|> 
--          do char '('
--             e <- expr
--             char ')'
--             return e

-- eval :: String -> Int
-- eval xs = fst (head (parse expr xs))