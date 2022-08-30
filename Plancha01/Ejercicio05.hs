module Ejercicio04 where

import Parsing

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Type = MInt Int | MChar Char deriving Show

hl2h :: Parser [Type]
hl2h = do (do string "[]"
              return [])
          <|>
          (do char '['
              r <- hl2h_1
              return r)

hl2h_1 :: Parser [Type]
hl2h_1 = do space --Trim izq, elimino los espacios
            (do i <- integer
                space
                ((do char ','
                     r <- hl2h_1
                     return ((MInt i):r))
                 <|>
                 (do char ']'
                     return [MInt i])
                 <|>
                 failure))
            <|>
            (do char '\''
                k <- item
                char '\''
                space
                ((do char ','
                     space
                     r <- hl2h_1
                     return ((MChar k):r))
                 <|>
                 (do char ']'
                     return [MChar k])
                 <|>
                 failure))

