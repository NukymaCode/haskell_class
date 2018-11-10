
module Haskell_Hoja1 where
import Data.Char

{- Ejercicios correspondientes a la HOJA 1 -}

{-
(a) Implementar una función en Haskell que dados tres números enteros determine si están
ordenados de menor a mayor.
-}
-- Ordenados de Mayor a meno
ordenadosMaMe::Integer->Integer->Integer->Bool
ordenadosMaMe a b c = if a >= b then 
						if b >= c then True
						else False
				 	  else False
			
-- Ordenados de menor a mayor
ordenadosMeMa::Integer->Integer->Integer->Bool
ordenadosMeMa a b c = if a <= b then 
						if b <= c then True
						else False
				  	  else False
				  	 
{-
(b) Implementar una función en Haskell que dados tres números enteros los devuelva
ordenados de menor a mayor.
-}
ordenar::Integer->Integer->Integer->[Integer]
ordenar a b c = ordenarAux a b c []

ordenarAux::Integer->Integer->Integer->[Integer]->[Integer]
ordenarAux a b c lista 
				| a <= b && a <= c = ordenarResto b c (a:lista)
				| b <= a && b <= c = ordenarResto a c (b:lista)
				| otherwise = ordenarResto a b (c:lista)
			
ordenarResto::Integer->Integer->[Integer]->[Integer]
ordenarResto x y lista 
				| x <= y = lista++[x,y]
				| otherwise = lista ++[y,x]
 			
{-
(c) Implementar en Haskell una función que reciba un número real y devuelva una tupla
con su parte entera y sus dos primeros decimales (como número entero).
-}
enteraDecimal::Float->(Integer, Integer)
enteraDecimal num = (truncate num, (truncate(num*100) - (truncate num)*100))

{-
(d) Crear una función que reciba el radio de una circunferencia y devuelva una 2-tupla con
la longitud de la circunferencia y con el área del círculo. Emplea una definición local con
la cláusula where para almacenar el valor de Pi (Nota: no se debe utilizar la función
predefinida pi). A continuación crear una función con el mismo cometido empleando la
definición local let.
-}
longAreaCirculo::Float->(Float,Float)
longAreaCirculo radio = 
				let
					perimetro = 2*mi_pi*radio
					area = mi_pi*radio^2
				in (perimetro,area)
				where mi_pi = 3.14159


-- LISTAS POR COMPRENSIÓN
{-
(e) Implementar la función predefinida de listas concat, que se llamará concatenar,
utilizando la definición de listas por comprensión (no se puede utilizar recursividad).
-}
myconcat::[[Int]]->[Int]
myconcat lista = [ x | sublista <- lista, x <- sublista]
		-- lista: lista original [[1,3,4],[2,7],[9,3]]
		-- sublista: sublista [1,3,4]
		-- x: elementos de la sublista x = 1

{-
(f) Implementar una función que dado un número entero devuelva en una lista todos los
factores de dicho número. Se debe utilizar la definición de listas por comprensión.
-}
factores::Integer->[Integer]
factores n = [ x | x <- [1..n], mod n x == 0 ]

{-
(g) Implementar una función que diga si un número es primo. Para ello se debe utilizar la
función que calcula el número de factores de un número (ejercicio f).
Nota: Si para resolver el ejercicio se deben comparar dos listas, se puede hacer con el
operador de igualdad de listas (==). Por ejemplo:
	> [1,2,3] == [1,2,3]
	True
	> [1,2,3]
-}
esPrimo::Integer->Bool
esPrimo n = if length(factores n)==2 then True
			else False
			
{-
(h) Implementar una función que diga cuántos caracteres en mayúscula están contenidos
en una frase dada. Se deberá utilizar la definición de listas por comprensión.
-}
cuantasMayusculas::[Char]->Int
cuantasMayusculas frase = length[ x | x <- frase, isUpper x == True]

-- --------------------------------------------------------------------------------------------------- --

-- AJUSTE DE PATRONES
{-
(i) Implementar una función que dada una tupla de tres elementos, donde cada uno de
ellos es a su vez una tupla de dos elementos de tipo String e Int respectivamente,
retorne el primer elemento de cada tupla interna. Se deberá utilizar ajuste de patrones.
-}
tresTupla::((Int,String),(Int,String),(Int,String))->(Int,Int,Int)
tresTupla ((x1,x2),(y1,y2),(z1,z2)) = (x1,y1,z1) 
	-- Si lo llamamos (X,Y,Z) sólo podremos trabajar con duplas.
	-- Podemos poner el widecar tresTupla ((x1,_),(y1,_),(z1,_)) = (x1,y1,z1) 
	
{-
(j) Implementar una función que devuelve True si la suma de los cuatro primeros
elementos de una lista de números enteros es un valor menor a 10 y devolverá False
en caso contrario. Se deberá utilizar ajuste de patrones.
-}
sumaDiez::[Integer]->Bool
sumaDiez (a:b:c:d:lista) 
					| (a+b+c+d)  < 10 = True
					| otherwise = False

{-
(k) Implementar una función que dado un carácter, que representa un punto cardinal,
devuelva su descripción. Por ejemplo, dado ‘N’ devuelva “Norte”.
-}
puntoCardinal::Char->String
puntoCardinal letra
				|letra == 'n' = "Norte"
				|letra == 's' = "Sur"
				|letra == 'e' = "Este"
				|letra == 'o' = "Oeste"
			--  |letra == 'j' || 'k' = "Caca"
				|otherwise = "No es un punto cardinal"

{-
(l) Implementar una función que dada una frase retorne un mensaje donde se indique cuál
es la primera y última letra de la frase original. Un ejemplo de aplicación de la función
podría ser:
-}
procesarFrase::[Char]->String
procesarFrase frase = "La primera letra es "++[head frase]++" y la ultima es "++[last frase]

{-
(m) Implementar una función que dado un número entero devuelva mensajes indicando en
qué rango de valores se encuentra dicho número (menor de 10, entre 10 y 20 o mayor
de 20). Se debe utilizar definiciones locales.
-}
rango::Integer->String
rango num = if num < 10 then mensaje1
			else if num < 20 then mensaje2 
					else mensaje3
			where
				mensaje1 = "menor de 10"
				mensaje2 = "entre 10 y 20"
				mensaje3 = "mqyor de 20"
				
{-
(n) Implementar una función que dada una cadena de caracteres y un carácter, indique el
número de apariciones del carácter en la cadena. No se debe utilizar recursividad, sí
ajuste de patrones. Pista: utilizar la definición de listas por comprensión					
-}
veces::[Char]->Char->Int
veces frase letra = length[ x | x <- frase, x == letra]


