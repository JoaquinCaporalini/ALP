module Ejercicio03 where

import Parsing

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

parentesis :: Parser a -> Parser a
parentesis p = (do char '('
                   r <- p
                   char ')'
                   return r)
                <|>
                p

parem :: Parser a -> Parser a
parem p = do{ char '(' ; r <- p ; char ')' ; return r } <|> p
       