module Ejercicio14 where

data BinTree a = Leaf | Bin a (BinTree a) (BinTree a) deriving Show
foldBin :: BinTree a -> b -> (a -> b -> b -> b) -> b
foldBin Leaf l b = l
foldBin (Bin a t u) l b = b a (foldBin t l b) (foldBin u l b)

-- Determina si un arbol es una hoja
isLeaf :: BinTree a -> Bool
isLeaf Leaf = True
isLeaf _    = False

-- Aplica la funcion argumento a todos los elementos de tipo a en el arbol
mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin _ Leaf = Leaf
mapBin f (Bin a l r) = Bin (f a) (mapBin f l) (mapBin f r)

-- Devuelve la altura del ´arbo
heightBin :: (Num p, Ord p) => BinTree a -> p
heightBin Leaf = 0
heightBin (Bin _ l r) = 1 + max (heightBin l) (heightBin r)


-- Devuelve el arbol espejo
mirrorBin :: BinTree a -> BinTree a
mirrorBin Leaf = Leaf
mirrorBin (Bin a l r) = Bin a (mirrorBin r) (mirrorBin l)