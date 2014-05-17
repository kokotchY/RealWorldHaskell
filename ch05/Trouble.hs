import Data.Char

upcaseFirst (c:cs) = toUpper c

camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))
