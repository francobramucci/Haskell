{-
 - A lo largo de una temporada un equipo de fútbol cada vez que un jugador metía un gol se anotaba su número de camiseta en una lista.
 - a) El cuerpo técnico del equipo quiere realizar una estadística y para ello quieren calcular la proporción entre la cantidad de
 - goles de tres jugadores determinados sobre el total de goles del equipo. 
 - Definir una función proporcion3 que calcule dicha proporción dada la lista de goles y el número de camiseta de tres jugadores. 
 - b) Para saber cuantos goles metio cada uno de esos tres jugadores se pide ahora definir una función goles3 que dados tres números de camiseta devuelva una tripla
 - con la cantidad correspondiente de goles de cada jugador según el orden en el que fueron pasados a la función.
 -
 - -}
ocurrencias3 :: [Int] -> Int -> Int -> Int -> (Int, Int, Int)
ocurrencias3 l a b c = foldr (\x (f, s, t) -> if x == a then (f+1, s, t) else if x == b then (f, s+1, t) else if x == c then (f, s, t+1) else (f, s, t)) (0,0,0) l

{- 
 - a) Una familia realizará un viaje por la ruta nacional 40 partiendo desde el kilómetro 0.
 - A lo largo del viaje pasarán por n >= 1 pueblos antes de llegar a destino. De antemano anotaron todos los kilómetros y los nombres de los pueblos del más cercano
 - al más lejano.
 - Como el viaje será muy largo, la familia quiere tomar un descanso luego de realizar el tramo entre dos pueblos más grande.
 - Definir una función tomarDescanso que con la información de la lista devuelva el nombre del pueblo donde la familia deberá realizar el descanso.   
 - b) La familia quiere calcular por cuantos pueblos podrán pasar con una cierta cantidad de combustible (sin cargar en el camino) 
 - sabiendo que un litro de nafta rinde 10km en la ruta.
 - Definir una función que dada la lista de pueblos y una cantidad de combustible devuelva la cantidad de pueblos por los que pudieron pasar.
 - -}

tomarDescanso :: [(String, Int)] -> String
tomarDescanso l = let (n, d, k) = foldl (\(s, max, ant) (pueblo, km) -> if km-ant >= max then (pueblo, km-ant, km) else (s, max, km)) ("Origen", 0, 0) l in n

mapfold :: (a -> b) -> [a] -> [b]
mapfold f l = foldr (\x ys -> f x : ys) [] l 

filterfold :: (a -> Bool) -> [a] -> [a]
filterfold f l = foldr (\x ys -> if f x then x:ys else ys) [] l
{-tomarDescanso :: [(String, Int)] -> Int -> (String, Int, Int)
tomarDescanso l = let dest = (fst (last l), 0, snd (last l)) in foldr (\(pueblo, km) (s, max, sig) -> if sig-km >= max then (pueblo, sig-km, km) else (s, max, km)) dest l
-}
