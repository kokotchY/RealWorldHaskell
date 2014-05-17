import Data.Bits (shiftL, (.&.), (.|.))
import Data.Char (digitToInt, toUpper, ord)
import Data.List

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

square :: [Double] -> [Double]
square (x:xs) = x*x : square xs
square [] = []

upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
upperCase [] = []

oddList :: [Int] -> [Int]
oddList (x:xs) | odd x = x : oddList xs
               | otherwise = oddList xs
oddList _ = []

mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc+x) xs
          helper acc _ = acc


base = 65521
adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b _ = (b `shiftL` 16) .|. a

adler32_try2 xs = helper (1,0) xs
    where helper (a,b) (x:xs) =
            let a' = (a + (ord x .&. 0xff)) `mod` base
                b' = (a' + b) `mod` base
            in helper (a',b') xs
          helper (a,b) _     = (b `shiftL` 16) .|. a

adler32_foldl xs = let (a, b) = foldl step (1, 0) xs
                   in (b `shiftL` 16) .|. a
    where
        step (a, b) x = let a' = a + (ord x .&. 0xff)
                        in (a' `mod` base, (a' + b) `mod` base)

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

asInt_fold :: String -> Int
asInt_fold [] = error "Empty string not allowed"
asInt_fold ['-'] = error "Symbol - not allowed"
asInt_fold ('-':xs) = - asInt_fold xs
asInt_fold l = if not ('.' `elem` l) then foldl (\acc x -> acc * 10 + digitToInt x) 0 l else error "Only int allowed"

concat' :: [[a]] -> [a]
concat' = foldr (\x acc -> x ++ acc) []


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (x:xs)
    | f x = x:takeWhile' f xs
    | otherwise = []

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f l = foldr (\x acc -> if f x then x:acc else []) [] l

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr (\x acc -> if f x then True else acc) False

cycle' :: [a] -> [a]
cycle' xs = foldr g [] [1..]
    where g _ ys = xs ++ ys

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

suffixes2 xs = init (tails xs)
