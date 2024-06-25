--Tema 1

maxi :: [Int] -> Int
maxi [x] = x
maxi (x:y:xs) = if (x > y) then maxi (x:xs) else maxi (y:xs)  

appendMax :: [Int] -> [Int]
appendMax [] = []
appendMax l = l ++ [maxi l]

prodcar2 :: [a] -> [b] -> [(a,b)]
prodcar2 xs ys = [(x,y) | x<-xs, y<-ys]

mapTriplas :: [(a,b,c)] -> (a -> b -> c -> d) -> [d]
mapTriplas xs f = [f x y z | (x,y,z) <- xs] 

--Ejemplo: mapTriplas [(1,2,3), (3,45,9), (5,3,1)] (\x y z -> x + y + z)

--Tema 2

mini :: [Int] -> Int
mini [x] = x
mini (x:y:xs) = if (x<y) then mini (x:xs) else mini (y:xs)

appendMin :: [Int] -> [Int]
appendMin [] = []
appendMin l = l ++ [mini l]

--prodcar2 idem

filterTriplas :: [(a,b,c)] -> (a -> b -> c -> Bool) -> [(a,b,c)]
filterTriplas xs f = [(x,y,z) | (x,y,z) <- xs, (f x y z)]

