maxi :: [Int] -> Int
maxi [x] = x
maxi (x:y:xs) = if (x > y) then maxi (x:xs) else maxi (y:xs)  

appendMax :: [Int] -> [Int]
appendMax [] = []
appendMax [x] = [x]
appendMax l = l ++ [maxi l]

prodcar2 :: [a] -> [b] -> [(a,b)]
prodcar2 xs ys = [(x,y) | x<-xs, y<-ys]

mapTriplas :: [(a,b,c)] -> (a -> b -> c -> d) -> [d]
mapTriplas xs f = [f x y z | (x,y,z) <- xs] 

--Ejemplo: mapTriplas [(1,2,3), (3,45,9), (5,3,1)] (\x y z -> x + y + z)
