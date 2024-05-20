head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

cons' :: [a] -> a -> [a]
cons' l x = x:l

snoc' :: [a] -> a -> [a]
snoc' [] y = [y]
snoc' [x] y = x:[y]
snoc' (x:xs) y = x : (snoc' xs y)

conssnoc' :: [a] -> a -> [a]
conssnoc' [] y = [y]
conssnoc' l y = y : (snoc' l y)

take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 l = []
take' n (x:xs) = x : (take' (n-1) xs)

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 l = l
drop' n (x:xs) = drop' (n-1) xs

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a : replicate' (n-1) a

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

insertNth :: Int -> a -> [a] -> [a]
insertNth n y [] = [y]
insertNth 0 y l = y:l
insertNth n y (x:xs) = x : (insertNth (n-1) y xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

prod' :: [Int] -> Int
prod' [] = 1
prod' (x:xs) = x * (prod' xs)

filterOdd' :: [Int] -> [Int]
filterOdd' [] = []
filterOdd' (x:xs) = if((mod x 2) == 1) then x : (filterOdd' xs) else filterOdd' xs

filterEven' :: [Int] -> [Int]
filterEven' [] = []
filterEven' (x:xs) = if((mod x 2) == 0) then x : (filterEven' xs) else filterEven' xs

zip' :: [a] -> [b] -> [(a,b)]
zip' l [] = []
zip' [] l = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

prodcar :: [a] -> [b] -> [(a,b)]
prodcar l [] = []
prodcar [] l = []
prodcar [x] (y:ys) = (x,y) : prodcar [x] ys
prodcar (x:xs) (y:ys) = (prodcar [x] (y:ys)) ++ (prodcar xs (y:ys))

singleton' :: a -> [a]
singleton' x = [x]

maxToEnd :: [Int] -> [Int]
maxToEnd [] = []
maxToEnd [x] = [x]
maxToEnd (x:y:xs) = if (x > y) then y: maxToEnd (x:xs) else x: maxToEnd(y:xs)































