{-
 - A lo largo de una temporada un equipo de fútbol cada vez que un jugador metía un gol se anotaba su número de camiseta en una lista.
 - a) El cuerpo técnico del equipo quiere realizar una estadística y para ello quieren calcular la proporción entre la cantidad de
 - goles de tres jugadores determinados sobre el total de goles del equipo. 
 - Definir una función proporcion3 que calcule dicha proporción dada la lista de goles y el número de camiseta de tres jugadores. 
 - b) Para saber cuantos goles metio cada uno de esos tres jugadores se pide ahora definir una función goles3 que dados tres números de camiseta devuelva una tripla
 - con la cantidad correspondiente de goles de cada jugador según el orden en el que fueron pasados a la función.
 -
 -}
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
 -
 -}

tomarDescanso :: [(String, Int)] -> String
tomarDescanso l = let (n, d, k) = foldl (\(s, max, ant) (pueblo, km) -> if km-ant >= max then (pueblo, km-ant, km) else (s, max, km)) ("Origen", 0, 0) l in n

mapfold :: (a -> b) -> [a] -> [b]
mapfold f l = foldr (\x ys -> f x : ys) [] l 

filterfold :: (a -> Bool) -> [a] -> [a]
filterfold f l = foldr (\x ys -> if f x then x:ys else ys) [] l
{-tomarDescanso :: [(String, Int)] -> Int -> (String, Int, Int)
tomarDescanso l = let dest = (fst (last l), 0, snd (last l)) in foldr (\(pueblo, km) (s, max, sig) -> if sig-km >= max then (pueblo, sig-km, km) else (s, max, km)) dest l
-}

{-
 - 3) Una organización sin fines de lucro quiere hacer un reconocimiento a sus mejores donantes.
 - La organización cuenta con registros donde se tiene el nombre del donante junto a todas las cantidades de dinero que donó a lo largo del tiempo 
 - (en orden cronológico).
 - Para aquellos casos en los que el donante decidió no decir su nombre, se anotó el monto donado en "Anónimo".
 - a) Diseñar una función que devuelva todos los nombres de aquellas personas que donaron más de un total "d" de dinero. No considerar el nombre "Anónimo" 
 - Un ejemplo de un registro es:
 - Juan: 100, 200, 400, 1000, 10.
 - b) Tras haber obtenido los mejores donantes, la organización quiere ahora invitar al que mayor cantidad donó de ellos para hacerle una distinción excepcional.
 - Definir una función que devuelva el mayor donante de los previamente elegidos.
 -
 -}


{- 
 - Dada una lista de valores booleanos devolver "tautología" si todos los valores son True, "satisfactible" si alguno de los valores es True y "contradicción" 
 - si todos los valores son falsos.
 - 
 -}

{- 
 - Tema 1: 
 - Dos alumnos se infiltraron en la oficina de su profesor para acceder a las planillas de notas de varios exámenes, en cada una de las planillas estaban
 - las notas que los alumnos habían sacado en el examen correspondiente. Uno de los alumnos quiere categorizar los exámenes del profesor según el siguiente criterio:
 - Difícil: Si el promedio de notas es menor a 4.
 - Fácil: Si el promedio de notas es mayor a 8.
 - Posible: El resto de los casos.
 - Diseñar una función en Haskell que realice dicha categoría si se tienen las listas de las notas de los alumnos de cada examen.
 -
 - Tema 2:
 - Difícil: Si la cantidad de aprobados es menor a 5.
 - Fácil: Si la cantidad de aprobados es mayor a 25.
 - Posible: El resto de los casos.
 - Diseñar una función en Haskell que realice dicha categoría si se tienen las listas de las notas de los alumnos de cada examen. (Considerando que en cada examen
 - rindieron 30 alumnos)
 -}




