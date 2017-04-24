--Ex 1(a)
checkprod::(Int,Int) -> Int ->Bool
checkprod  (n,m) i  | n >= 0 && m >= 0 && n*m <= i = True -- if condition holds return true
                    | otherwise = False

--Ex 1(b)
smallerprod::Int -> [(Int,Int)]
smallerprod i = [(n,m) | n <- [1..i], m <- [1..n], n*m <= i]

-- Ex 2(a)

prime :: Integer -> Bool
prime i = length [x | x <- [1..i], mod i x == 0] == 2 --if the length of the list containing this numbers factors is only two it is prime

-- Ex 2(b)
primes::[Integer]
primes = [x | x <- [1..], prime x]

-- Ex 3
double :: String -> String
double [] = []
double [x] = [x]++[x]
double (x:xs) = [double [x]]++[double [xs]]

-- Ex 4
--cycleinc::[Integer] -> [Integer]
--cycleinc [] = []
--cycleinc [x] = [i | i <- [x..x+1]]
--cycleinc (x:xs) = [i | i <- [x..(cycleinc xs)]]

-- Ex 5
vecSqDist :: [Int] -> [Int] -> Int
vecSqDist = undefined
