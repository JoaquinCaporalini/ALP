module Ejercicio07 where

import Parsing

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

-- data Basetype = DInt | DChar | DFloat deriving Show
-- type Hasktype = [Basetype]
data Hasktype = DInt | DChar | DFloat | Fun Hasktype Hasktype deriving Show

functionType :: Parser Hasktype
functionType = do space --Lo esta haciendo???
                  (do (string "Int")
                      space
                      ((do char '-'
                           char '>'
                           space
                           r <- functionType
                           return (Fun DInt r))
                       <|>
                       return DInt)
                      )
                  <|>
                  (do (string "Char")
                      space
                      ((do char '-'
                           char '>'
                           space
                           r <- functionType
                           return (Fun DChar r))
                       <|>
                       return DChar)
                      )
                  <|>
                  (do (string "Float")
                      space
                      ((do char '-'
                           char '>'
                           space
                           r <- functionType
                           return (Fun DFloat r))
                       <|>
                       return DFloat)
                      )
                  <|>
                  (do alphanum
                      failure)


-- parse functionType "Int -> Char -> Float"