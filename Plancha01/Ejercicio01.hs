import Parsing


parse space "    5" -- Trim izquierda

a = sepBy item space
parse a "h o l a"


b = sepBy item item
parse a "h o l a"
