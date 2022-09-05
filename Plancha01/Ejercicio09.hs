module Ejercicio09 where

import Parsing

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Pointer = P | AP Array
data Array = V | A Int
data Type = DInt | DChar | DFloar
data CType =   CInt Pointer Array
             | CChar Pointer Array
             | CFloar Pointer Array 

data Dec = Var String | Static Dec Int | PointerTo Dec 
-- declaration -> type_specifier declarator ';'
declaration :: Parser CType
declaration = do space
                 t <- type_specifier
                 space


-- declarator -> '*' declarator | direct_declarator
declarator ::
-- direct_declarator -> direct_declarator '[' constant_expression ']' | '(' direct_declarator ')' | identifier
-- type_specifier -> 'int' | 'char' | 'float'
type_specifier = space
                 (do )
                 <|>
                 (do )
                 <|>
                 (do )
-- constant_expression -> number
constant_expression :: Int
constant_expression = integer
