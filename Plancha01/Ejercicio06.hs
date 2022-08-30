module Ejercicio06 where

import Parsing

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype ]

functionType :: Parser Hasktype
functionType = do space --Lo esta haciendo???
                  (do (string "Int")
                      space
                      ((do char '-'
                           char '>'
                           space)
                       <|>
                       space)
                      r <- functionType
                      return (DInt:r)
                      )
                  <|>
                  (do (string "Char")
                      space
                      ((do char '-'
                           char '>'
                           space)
                       <|>
                       space)
                      r <- functionType
                      return (DChar:r)
                      )
                  <|>
                  (do (string "Float")
                      space
                      ((do char '-'
                           char '>'
                           space)
                       <|>
                       space)
                      r <- functionType
                      return (DFloat:r)
                      )
                  <|>
                  (do alphanum
                      failure)
                  <|>
                  (do return [])


-- parse functionType "Int -> Char -> Float"