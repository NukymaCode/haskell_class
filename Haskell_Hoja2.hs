
module Haskell_Hoja2 where
import Data.List

{- Ejercicios correspondientes a la HOJA 2 -}

{-
(a) Implementa una función en Haskell que elimine de una lista de enteros aquellos
números múltiplo de x.
	> cribar [0,5,8,9,-9,6,0,85,-12,15] 2
	[5,9,-9,85,15]
		Se piden diferentes versiones de la misma función:
		- Con definición de listas por comprensión
		- Con recursividad no final
		- Con recursividad final o de cola
-}

-- Con definición de listas por comprensión
cribar::[Integer]->Integer->[Integer]
cribar lista num = [ x | x <- lista, x /= num ]

-- Con recursividad no final. La ultima operación no es la recursiva.
cribar'::[Integer]->Integer->[Integer]
cribar' [] _ = []
cribar' (l:ls) num 
		| l /= num = [l]++(cribar' ls num)
		| otherwise = cribar' ls num 

-- Con recursividad final o de cola. La ultima operación es una recursiva.
-- Si necesitas acumulador, o función auxiliar -> RECURSIVIDAD FINAL
cribar''::[Integer]->Integer->[Integer]
cribar'' [] _ = []
cribar'' lista num = cribarAux lista num []

cribarAux::[Integer]->Integer->[Integer]->[Integer]
cribarAux (l:ls) num acum 
		| l /= num = cribarAux ls num [l]++acum
		| otherwise = cribarAux ls num acum

{-
(b) Dada la siguiente definición de función
	doble :: Int -> Int
	doble x = x + x
¿Cómo cambiaría la definición utilizando expresiones lambda?
-}
doble::Int->Int
doble = (\x -> x+x)
	
{-
(c) Se pide una función en Haskell que dada una lista de números enteros obtenga un
número entero con el resultado de calcular el doble de cada uno de los elementos de la
lista original y sumarlos todos. Se piden diferentes versiones de la misma función:
	- Con recursividad no final
	- Con recursividad final o de cola
	- Utilizando expresiones lambda u orden superior (se puede hacer uso de la función predefinida de Haskell map).
		> sumaDobles [2,3,4]
		18
		> sumaDobles [1,2,3]
		12
-}
-- Recursividad no final ( no vale aux ni acumuladores)
sumaDobles::[Integer]->Integer
sumaDobles [] = 0
sumaDobles (x:xs) = sum([x*2]++[sumaDobles xs])

-- Recursividad Final (usamos aux y acumulador)
sumaDobles'::[Integer]->Integer
sumaDobles' [] = 0
sumaDobles' lista =  sumaDoblesAux lista 0

sumaDoblesAux::[Integer]->Integer->Integer
sumaDoblesAux [] acumulador = acumulador
sumaDoblesAux (l:ls) acum = sumaDoblesAux ls (l*2)+acum

-- Utilizando expresiones orden superior
sumaDobles''::[Integer]->Integer
sumaDobles'' lista = sum (map (*2) lista)
		-- map f xs -> obtienes una lista resultado de aplicar la funcion f a cada elemento de la lista

-- Utilizando expresiones lambda + plegado
sumaDobles'''::[Integer]->Integer
sumaDobles''' lista = foldl(\ acumulador x -> (x*2)+acumulador) 0 lista


{-
(d) Implementa una función que sume los cuadrados de los números pares contenidos en
una lista de números enteros. Se piden dos versiones:
	a. 	Una versión que haga uso de las funciones de orden superior de listas map y
		filter para definir la nueva función.
	b. 	Una versión que utilice la definición de listas por comprensión.
-}

-- Map y Filter
sumaCuadradosPares'::[Integer]->Integer
sumaCuadradosPares' lista = sum (map (^2) (filter (even) lista))

-- Listas por Comprensión
sumaCuadradosPares::[Integer]->Integer
sumaCuadradosPares [] = 0
sumaCuadradosPares lista = sum[ x^2 | x <- lista, x `mod` 2 == 0]


{-
(e) Dada una lista de enteros, implementar una función para devolver tuplas formadas por
los elementos (sin repetir) de la lista, junto con la primera posición en la que aparecen.
-}

primerAparicion::[Integer]->[(Integer, Integer)]
primerAparicion lista = primerAux lista 0 [] []
		-- 	Necesitamos llevar la lista original, la posición, la lista de repetidos y la lista de solución

primerAux::[Integer]->Integer->[Integer]->[(Integer, Integer)]->[(Integer, Integer)]
primerAux [] _ _ sol = sol
primerAux (n:ns) posicion repes sol = 	if pertenece repes n then primerAux ns (posicion+1) repes sol
									    else primerAux ns (posicion+1) (n:repes) (sol++[(n,posicion)])

-- Esta función pertenece sirve para cualquier tipo							    
pertenece::(Eq a)=>[a]->a->Bool 
pertenece [] _ = False
pertenece (l:ls) elemento = if l==elemento then True
							else pertenece ls elemento
	
						 
{-
(f) Implementar en Haskell una función que calcule el número de secuencias de ceros que
hay en una lista de números.
-}
numeroSecuencias::[Integer]->Integer->Integer
numeroSecuencias lista numero = numeroSecuenciasAux lista numero 0

numeroSecuenciasAux::[Integer]->Integer->Integer->Integer
numeroSecuenciasAux [] _ contador = contador
numeroSecuenciasAux [0] 0 contador = contador+1
numeroSecuenciasAux (a:b:ls) num contador = if a==num && b/=num then numeroSecuenciasAux (b:ls) num (contador+1)
										    else numeroSecuenciasAux (b:ls) num contador

-- El truco es contar la secuencia cuando termina (secuencia de ceros)
secuencia::[Int]->Int
secuencia[0] = 1
secuencia[x] = 0
secuencia(0:0:xs) = secuencia (0:xs)
secuencia(0:x:xs) = 1 + secuencia xs
secuencia(y:x:xs) = secuencia (x:xs)

{-
(g) Implementar una función en Haskell que reciba una lista de números enteros y devuelva
dos listas: una con los elementos sin repetir y otra con los elementos que están repetidos.
-}
separarLista::[Int]->([Int],[Int])
separarLista lista = foldl (\ ( unicos, repes)  x -> if pertenece repes x then (unicos,repes)
                                                     else if (pertenece unicos x) then (borrar unicos x, repes ++[x])
                                                     else (unicos++[x],repes) ) ([ ],[ ] ) lista
                                                                        
-- BORRAR un elemento de una lista
borrar::(Eq a)=>[a]->a->[a]
borrar lista elemento = foldl(\ acumulador x -> if x==elemento then acumulador else acumulador++[x]) [] lista   
                                    -- x es el elemento x de la lista
                                    -- acumulador es la lista que voy a devolver
                                    -- acum++[x]  se lo sumas por la derecha porque tenemos que devolver la lista tal y como está                                                        


{-
(h) Dada una lista de números enteros implementar una función que devuelva 
una lista con los n elementos mayores de la lista original.
-}
	-- Voy a optar por ordenar una lista de mayor de a menor y luego usar el TAKE para coger los n elementos
		{-
		Obtener una lista con los n primeros elementos de la lista original 
		(take n lista) o una lista eliminando los n primeros (drop n lista). 
		El argumento n debe ser un entero no negativo
		-}
		
nMayores::[Int]->Int->[Int]
nMayores lista n = take n (sort lista)

{-
(j) Dada una lista de enteros, se pide implementar una función que ordene dicha lista de
menor a mayor utilizando un algoritmo de inserción. Dicho algoritmo de inserción
consiste en recorrer la lista L, insertando cada elemento L[i] en el lugar correcto entre
los elementos ya ordenados L[1] ,...,L[i-1].
-}
--ordenarInsercion::[Integer]->[Integer] 
--ordenarInsercion lista = foldl (\ acumulador x -> ordenarInsercionAux x acumulador) [] lista

ordenarInsercion lista = ordenarInsercionAux lista []

ordenarInsercionAux::[Integer]->[Integer]->[Integer]
ordenarInsercionAux [] [] =[]
ordenarInsercionAux [] acum = acum
ordenarInsercionAux (x:xs) (a:b:ls) = 	if x<=a then ordenarInsercionAux xs ([x]++[a]++[b]++ls)
										else if x>=b then ordenarInsercionAux xs ([a]++[b]++[x]++ls)
										else ordenarInsercionAux xs ([a]++[x]++[b]++ls)
										
ordenarInsercion'::[Integer]->[Integer] 			
ordenarInsercion' (x:xs) = ordenarInsercion' xs 
			
insertarMedio::Integer->[Integer]->[Integer]
insertarMedio x (a:b:ls) = if x<=a then [x]++[a]++[b]++ls
							else if x>=b then [a]++[b]++[x]++ls
							else [a]++[x]++[b]++ls

{-
(k)Implementa una función polimórfica en Haskell que reciba 2 listas y vaya cogiendo un
elemento de la primera y dos de la segunda, creando una lista final de ternas. En caso
de que una de las dos listas se acabe, mostrará la lista de ternas construidas hasta ese
momento.
-}   
mezclarEnTernas::[Integer]->[Integer]->[(Integer,Integer,Integer)]
mezclarEnTernas [] _ = []
mezclarEnTernas _ [] = []
mezclarEnTernas _ [a] = []
mezclarEnTernas (x:xs) (y:z:zs) = (x,y,z):(mezclarEnTernas xs zs)      

{-
(l) Se pide una función polimórfica en Haskell que dado un elemento y una lista añada
dicho elemento al final de la lista.  
-}       
alFinal::[a]->a->[a]
alFinal lista elemento = lista++[elemento]

{-
(m) Mediante la programación de orden superior se pide implementar una de las funciones
predefinidas en la librería estándar de Haskell: la función zipWith. Esta función recibe
como parámetros una función y dos listas y une ambas listas aplicado la función entre
los correspondientes parámetros.
	> zipWith' max [6,3,2,1] [7,3,1,5]
	[7,3,2,5]
	> zipWith' (++) ["hola ", "ciao ", "hi "] ["pepe", "ciao", "peter"]
	["hola pepe","ciao ciao","hi peter"]
	ghci> zipWith' (*) (replicate 5 2) [1..]
	[2,4,6,8,10]
	> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6]] [[3,2,2],[3,4,5]]
	[[3,4,6],[9,20,30]]
	> zipWith’ crearTupla [1,2,3] "casita"
	[(1,'c'),(2,'a'),(3,'s')]
	
	(Suponiendo que la función crearTupla tiene la siguiente definición:
	crearTupla :: a-> b-> (a,b)
	crearTupla x y = (x,y)
	)
-}

{-
(n) Define una función polimórfica que sea capaz de invertir los elementos de una lista. Se
piden diferentes versiones:
	- Con recursividad no final
	- Con recursividad de cola o final
	- Utilizando la función de orden superior foldr
	> reverse' [1,2,3]
	[3,2,1]
	> reverse' "casa"
	"asac"
-}
-- FUNCIONES LAMBDA
-- Invertir elementos de la lista
invertir::[Int]->[Int]
invertir lista = foldl (\acum x-> x:acum) [] lista
                -- x:acum para meter los elementos por la derecha
                -- Con el parametro acum ya estamos metiendo el caso base. No tiene elemento que sacar, nos devuelve directamente la lista vac�a

-- Utilizando la función de orden superior foldr
invertir'::[a]->[a]
invertir' lista = foldr(\ x acumulador -> acumulador++[x]) [] lista

{-
(o) Define una función polimórfica que sea capaz de invertir los elementos de una lista de
listas.
	> reverse'' [[1,2,3],[3,4,5]]
	[[5,4,3],[3,2,1]]
	> reverse'' ["pepe", "casa", "patio"]
	["oitap","asac","epep"]
-}

{-
(p) Implementar la función predefinida de la librería estándar flip. Esta función lo que
hace es recibir una función y devolver otra función que es idéntica a la función original,
salvo que intercambia los dos primeros parámetros.
	> flip' zip [1,2,3] "casa"
	[('c',1),('a',2),('s',3)]
	> flip' (+) 3 4
	7
	> flip' (++) "casa" "pollo"
	"pollocasa"
-}

{-
(q) Implementar la función polimórfica predefinida de la librería estándar map. Esta función
lo que hace es recibir una función y una lista y devuelve la lista resultante de aplicar la
función a cada elemento de la lista original.
	> map (3*) [1,2,3]
	[3,6,9]
	> map doble [1,2,3]
	[2,4,6]
	> map not [True,False]
	[False,True]
 -}