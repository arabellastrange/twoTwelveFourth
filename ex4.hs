
--Exercise 1
cutoffat :: Eq a => a -> [a] -> [a]
cutoffat a [] = []
cutoffat a (x:xs)   | x == a = [x]
                    | x /= a = [x] ++ cutoffat a xs

--Exercise 2
gencollatzdist :: (Int -> Int) -> Int -> Int
gencollatzdist f x | x == 1 = 0
                   | f x == 1 = 1
                   | f x /= 1 = 1 + gencollatzdist f (f x)

--Exercise 3

countocc :: Char -> String -> Int
countocc a xs = foldr (checkChar a) 0 xs

checkChar :: Char -> Char -> Int -> Int
checkChar a b y | a == b = y + 1
                | a /= b = y

--Exercise 4
--(a)
insert :: Ord b => (a -> b) -> a -> [a] -> [a]
insert f a [] = [a]
insert f a [x] | f x > f a = [a, x]
               | f x < f a = [x, a]
insert f a (x:xs) | f x > f a = [a, x] ++ insert f a xs
                  | f x < f a = [x] ++  insert f a xs

--(b)
inssort :: Ord b => (a -> b) -> [a] -> [a]
inssort = undefined

--(c) Order a list of strings according to length
sortwords :: Char -> [String] -> [String]
sortwords = undefined
