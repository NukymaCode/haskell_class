
module Haskell_EjerciciosExtraArboles where

import Data.Char
import Data.List

data Arbol' a = AV|Nodo a (Arbol' a) (Arbol' a)  deriving (Eq,Ord,Show)

-- ___________________________________________________________________________________________________________ --
--Numero Nodos
-- ___________________________________________________________________________________________________________ --
numNodos :: Arbol' a -> Int
numNodos AV = 0
numNodos (Nodo r izq der) = 1 + numNodos izq + numNodos der

-- ___________________________________________________________________________________________________________ --
--Numero Hojas
-- ___________________________________________________________________________________________________________ --
numHojas :: Arbol' a -> Int
numHojas AV = 0
numHojas (Nodo _ AV AV) = 1 
numHojas (Nodo _ izq der) = numHojas izq + numHojas der

-- ___________________________________________________________________________________________________________ --
--Altura arbol
-- ___________________________________________________________________________________________________________ --
altura::Arbol' a -> Int
altura AV = 0
altura (Nodo a izq der)= 1 + maximo(altura (izq),altura(der))	
								
maximo::(Int,Int)->Int
maximo(a,b)= if a>b then a else b	

-- ___________________________________________________________________________________________________________ --	
--Insertar en Arbol Bin Busq
-- ___________________________________________________________________________________________________________ --
insertar::(Ord a)=>Arbol' a-> a-> Arbol' a
insertar AV a =(Nodo a AV AV)
insertar (Nodo r iz der) elemento = if r>elemento then (Nodo r (insertar iz elemento) der)
									else (Nodo r iz (insertar der elemento))
									
-- ___________________________________________________________________________________________________________ --									
--Recorrido Inorden
-- ___________________________________________________________________________________________________________ --
recIn:: Arbol' a ->[a]
recIn AV=[]
recIn (Nodo a izq der)= recIn(izq)++[a]++ recIn(der)

-- ___________________________________________________________________________________________________________ --
-- Reconstruir un árbol a partir de dos listas
-- ___________________________________________________________________________________________________________ --
reconstruir::(Eq a)=>[a]->[a]->Arbol' a 
reconstruir [][] = AV
reconstruir (p:ps) inorden = Nodo p (reconstruir pri ini)(reconstruir prd ind)
							 where 
								(ini,ind) = partirl inorden p []
								(pri,prd) = partirD ps (length ini) []

partirl :: (Eq a)=>[a]->a->[a]->([a],[a])
partirl (x:xs) p acum = if x == p then (acum,xs) else partirl xs p (acum ++[x])

partirD :: [a]->Int->[a]->([a],[a])
partirD (x:xs) elementos acum = if elementos == 0 then (acum,xs) else partirD xs (elementos -1) (acum++[x])



-- ___________________________________________________________________________________________________________ --
-- EJERCICIO E - SACAR ARBOL POR PANTALLA
-- ___________________________________________________________________________________________________________ --

{- ANA
	c) Dado un nuevo tipo de datos para representar un árbol binario de cualquier tipo, definido como sigue:
	
		data Arbol a = AV | Rama (Arbol a) a (Arbol a)
	
	Se pide definir una función que visualice el árbol por pantalla de una determinada forma:
	separando cada hijo izquierdo y derecho por “|”, la raíz entre guiones y cada nivel diferente
	del árbol por “( )”. 
	
	Ejemplos de aplicación de la función sería los siguientes:
		> mostrarArbol (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
		"((60)|-8-|())|-5-|(4)"
		> mostrarArbol (Rama AV 5 (Rama AV 4 AV))
		"()|-5-|(4)"
		
	¿Sería equivalente a declarar el nuevo tipo de datos Arbol como una instancia de la clase
	Show?
	data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
-}

-- ___________________________________________________________________________________________________________ --
-- EJERCICIO D - ARBOL BINARIO ESPEJO
-- ___________________________________________________________________________________________________________ --

{- ANA
	d) Dado el siguiente tipo de datos que representa un árbol binario:
	
		data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
	
	Se pide definir una función que calcule el espejo de un árbol.
	
	Ejemplos de aplicación de la función serían:
		> espejo (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV))
		Rama (Rama AV 4 AV) 5 (Rama AV 8 (Rama AV 60 AV))
		> espejo (Rama AV 5 (Rama AV 4 AV))
		Rama (Rama AV 4 AV) 5 AV
-}

data Arbol a = ET | Rama (Arbol a) a (Arbol a)  deriving (Eq,Ord,Show)
-- ___________________________________________________________________________________________________________ --
-- DECIR SI DOS ÁRBOLES SON IGUALES:
-- ___________________________________________________________________________________________________________ --
iguales::Arbol Char -> Arbol Char ->Bool
iguales ET ET = True
iguales ET (Rama a b c)= False
iguales (Rama a b c) ET= False
iguales (Rama izq1 n1 der1) (Rama izq2 n2 der2) = n1==n2

-- ___________________________________________________________________________________________________________ --

mi_arbol::Arbol Integer
-- mi_arbol = Rama (Rama ET 4 ET) 5 (Rama ET 8 (Rama ET 60 ET))
mi_arbol = Rama (Rama (Rama ET 1 ET) 2 (Rama ET 3 (Rama ET 4 ET))) 5 (Rama (Rama ET 6 (Rama ET 7 ET)) 8 (Rama (Rama (Rama ET 9 ET) 10 (Rama ET 11 ET)) 12 (Rama ET 13 ET)))
mi_arbol2 = Rama (Rama (Rama ET 1 ET) 2 (Rama ET 3 (Rama ET 4 ET))) 5 (Rama (Rama ET 6 (Rama ET 7 ET)) 8 (Rama (Rama (Rama ET 9 ET) 10 (Rama ET 11 ET)) 12 (Rama ET 13 ET)))

-- ___________________________________________________________________________________________________________ --

{- Árbol binario - Recorridos:
	Preorden: (raíz, izquierdo, derecho). Para recorrer un árbol binario no vacío en preorden, 
	hay que realizar las siguientes operaciones recursivamente en cada nodo, comenzando con el nodo de raíz:
		Visite la raíz
		Atraviese el sub-árbol izquierdo
		Atraviese el sub-árbol derecho
	Inorden: (izquierdo, raíz, derecho). Para recorrer un árbol binario no vacío en inorden (simétrico), 
	hay que realizar las siguientes operaciones recursivamente en cada nodo:
		Atraviese el sub-árbol izquierdo
		Visite la raíz
		Atraviese el sub-árbol derecho
	Postorden: (izquierdo, derecho, raíz). Para recorrer un árbol binario no vacío en postorden, 
	hay que realizar las siguientes operaciones recursivamente en cada nodo:
		Atraviese el sub-árbol izquierdo
		Atraviese el sub-árbol derecho
		Visite la raíz
-}

recorridoPreorden:: Arbol a -> [a]
recorridoPreorden ET = []
recorridoPreorden (Rama izq n der) = [n]++recorridoPreorden(izq)++recorridoPreorden(der)
		
recorridoInorden:: Arbol a -> [a]
recorridoInorden ET = []
recorridoInorden (Rama izq n der) = recorridoInorden(izq)++[n]++recorridoInorden(der)

recorridoPostorden:: Arbol a -> [a]
recorridoPostorden ET = []
recorridoPostorden (Rama izq n der) = recorridoPostorden(izq)++recorridoPostorden(der)++[n]				
