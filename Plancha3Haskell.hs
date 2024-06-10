suma' x 0 = x
suma' x y = 1 + suma' x (y-1)

producto' x 0 = 0
producto' x 1 = x 
producto' x y = suma' x (producto' x (y-1))

potencia' x 0 = 1
potencia' x 1 = x
potencia' x y = producto' x (potencia' x (y-1))

sumaN 1 = 1
sumaN n = suma' n (sumaN (n-1))

interval' n m | ((n+1) == m || n == m) = [] | n < m = (n+1) : (interval' (n+1) m) 

fact' 0 = 1
fact' k = producto' k (fact' (k-1))

fib' 1 = 1
fib' 2 = 1
fib' n = suma' (fib' (n-2)) (fib' (n-1))

fiblist' 1 = [1]
fiblist' k = fib' k : (fiblist' (k-1))

divisores k 0 = 1
divisores k 1 = 0
divisores k s = if (mod k s) == 0 then 1 + (divisores k (s-1)) else (divisores k (s-1)) 
prime' k = (divisores k k-1 == 0) 

partialSums' 0 = []
partialSums' k = (sumaN k) : (partialSums' (k-1))  


