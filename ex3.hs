-- Exercise 1 (a)

mysearch :: Eq a => a -> [a] -> Bool
mysearch a [] = False
mysearch a [x] = a == x
mysearch a (x:xs) = a == x || (mysearch a xs)

-- Exercise 1 (b)

myreverse :: [a] -> [a]
myreverse []  = []
myreverse [x] = [x]
myreverse (x:xs) = (myreverse xs) ++ [x]

--Exercise 2

data MoneyAmount = Amount Float Currency
     deriving (Show, Eq)
data Currency = EUR | GBP | USD | CHF
     deriving (Show, Eq)
type Account = (Integer, MoneyAmount)

updateCurr :: (Float -> Float) -> Currency -> Account -> Account
updateCurr func newCurr (accNo, (Amount money oldCurr)) = (accNo, (Amount (func money) newCurr))

--Exercise 3

type Position = (Int,Int)
data Step = L | R | U | D

--(a)
move::Step->Position->Position
move L (x,y) = (x - 1, y)
move R (x,y) = (x + 1, y)
move U (x,y) = (x, y + 1)
move D (x,y) = (x, y - 1)

--(b)
type Route = [Step]

positions::Route -> Position->[Position]
positions [] (x,y) = [(x,y)];
positions [s] (x,y) = [(x,y)] ++ [move s (x,y)]
positions (s:r) (x,y) = [(x,y)] ++ positions r (move s (x,y))

--(c)
equiv :: Route -> Route -> Bool
equiv [][] = True
equiv (x:xs) (y:ys) = last (positions (x:xs) (0,0)) == last (positions (y:ys) (0,0))
