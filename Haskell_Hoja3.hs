
module Haskell_Hoja3 where
import Data.Char
import Data.List

-- ___________________________________________________________________________________________________________ --
{-
(a) Se pide una funci�n que dada una lista de racionales, donde cada racional se define como dos n�meros enteros 
(numerador y denominador), y un n�mero racional, devuelva otra lista con todos los racionales equivalentes al dado. 
-}

type Numerador = Integer
type Denominador = Integer
data Racional'  = R'{num::Numerador, den::Denominador} deriving Show

equivalente'::[Racional']->Racional'->[Racional']
equivalente' lista rac = foldl(\ acum r -> if num r * den rac == num rac * den r then [r]++acum
										 else acum) [] lista
									
-- ___________________________________________________________________________________________________________ --
{-
(b) Se pide varias funciones para hacer lo siguiente:

	1. Funci�n que dado un punto de coordenadas y una direcci�n (Norte, Sur, Este u Oeste) mueva el punto hacia la direcci�n indicada. 
		Un ejemplo de aplicaci�n de la funci�n ser�a:
			> mover Este (3,4) ---> (4,4)
			> mover Norte (3.5,9.2)---> (3.5,10.2)
			
		
	2. Funci�n que dados dos puntos de coordenadas indique cu�l est� m�s al sur.
		Ejemplos de aplicaci�n de la funci�n son:
			> masAlSur (3,5) (4,6) ---> (3.0,5.0)
			> masAlSur (4.5,-6.2) (4.5,-7) ---> (4.5,-7.0)
			
	3. Funci�n que calcule la distancia entre dos puntos:
			> distancia (3,5) (6,7) ---> 3.6055512
			
	4. Funci�n que dado un punto y una lista de direcciones, retorne el camino que forman todos los puntos despu�s de cada 
	movimiento sucesivo desde el punto
	original:
			> camino (3.2,5.5) [Sur,Este,Este,Norte,Oeste]
			[(3.2,4.5),(4.2,4.5),(5.2,4.5),(5.2,5.5),(4.2,5.5)]

-}

-- Creamos un tipo de dato que es COORDENADA, que consta de una X y una Y, ambos INTEGERS
type X = Integer
type Y = Integer
data Coordenada = Punto X Y deriving Show

-- Creamos un tipo de dato enumerado que es DIRECCION, consta de los cuatro valores de los puntos cardinarles
data Direccion = Norte | Sur | Este | Oeste deriving Show

-- b) 1
-- Implementamos el m�todo MOVER. Le pasamos una DIRECCI�N y una coordenada 
-- y nos devuelve la coordenada resultante de habernos movido en esa DIRECCION
mover::Direccion->Coordenada->Coordenada
mover Norte (Punto x y)=Punto x (y+1)
mover Sur (Punto x y)=Punto x (y-1)
mover Este (Punto x y)=Punto (x+1) y
mover Oeste (Punto x y)=Punto (x-1) y

-- b) 2 
-- Dados dos puntos (COORDENADAS) indicamos cual est� m�s al SUR. Est� m�s al Sur el que tenga la y m�s peque�a
masAlSur::Coordenada->Coordenada->Coordenada
masAlSur (Punto x1 y1) (Punto x2 y2) = if y1<y2 then (Punto x1 y1) else (Punto x2 y2)

-- b) 3
-- Distancia entre dos puntos
{- 
	For example, given an Int value n, one does not simply take its square root by typing sqrt n, 
	since sqrt can only be applied to Floating-point numbers. 
	Instead, one must write sqrt (fromIntegral n) to explicitly convert n to a floating-point number. 
	There are special cases for converting from Integers:
	fromInteger :: Num a => Integer -> a
	as well as for converting to Integers:
	toInteger:: Integral a => a -> Integer 
-}
distancia::Coordenada->Coordenada->Float
distancia (Punto x1 y1) (Punto x2 y2) = sqrt( (fromInteger (x1-x2))^2 + (fromInteger (y1-y2))^2 )

-- b) 4
-- Retornar una lista de coordenadas con el camino 
-- > camino (3.2,5.5) [Sur,Este,Este,Norte,Oeste] ---> [(3.2,4.5),(4.2,4.5),(5.2,4.5),(5.2,5.5),(4.2,5.5)]


camino::Coordenada->[Direccion]->[Coordenada]					
camino (Punto x1 y1) [] = [(Punto x1 y1)] 					-- No tengo Direcciones donde moverme
camino (Punto x1 y1) (m:movimientos) = foldl(\ acum m -> acum++[mover m (last acum)])[mover m (Punto x1 y1)] movimientos
-- De entrada tengo que meter la coordenada origen, de la que partimos en la lista. 
-- Por eso ponemos al final [mover mov (Punto x1 y1)] (As� tenemos el punto de partida en la lista acum que se devolver�)
-- Lo necesitamos en esa lista, porque es sobre el punto que vamos a calcular el siguiente movimiento.

-- Lo que vas metiendo en el acum es el resultado de realizar el movimiento m (sacado de la lista de direcciones) sobre la coordenada anterior
-- Esta coordenada anterior la tienes en acum y la sacamos con (last acum).
-- Por eso necesitamos meter en el caso base la coordenada de la que partimos, para poder hacer el movimiento sobre esta y que 
-- cuando hagamos (last acum) la primera vez, encontremos ah� la coordenada original

	{- EJECUCION
		*T5P1TypesData> camino (Punto 3 5) [Norte,Este,Sur,Oeste]
		[Punto 3 6,Punto 4 6,Punto 4 5,Punto 3 5]
	-}
	
-- ___________________________________________________________________________________________________________ --

{-
(c) Definir una funci�n que dado un d�a de la semana, indique si �ste es o no laborable. 
	Para representar el d�a de la semana se deber� crear un nuevo tipo enumerado.
-}
data Diasemana = L | M | X | J | V | S | D deriving (Show,Eq)

esLaborable::Diasemana->Bool
esLaborable dia = if (dia==S) then False
				  else if (dia==D) then False
				       else True
					
esLaborable'' :: Diasemana -> Bool
esLaborable'' dia
			| dia==D = False
			| dia==S = False
			| otherwise = True
			
-- ___________________________________________________________________________________________________________ --

{-
	(d) La empresa RealTimeSolutions, Inc. est� trabajando en un controlador para una central
	dom�tica. El controlador recibe informaci�n de termostatos situados en diferentes
	habitaciones de la vivienda y bas�ndose en esta informaci�n, activa o desactiva el aire
	acondicionado en cada una de las habitaciones. Los termostatos pueden enviar la
	informaci�n sobre la temperatura en grados Celsius o Fahrenheit. A su vez, los aparatos de
	aire acondicionado reciben dos tipos de �rdenes: apagar y encender (on y off). Se pide:
-}

-- (d) 1. Definir un tipo de datos para representar las temperaturas en ambos tipos de unidades.

data Grados = Celsius | Fahrenheit deriving (Show,Eq)
data Temperatura = Temperatura {valor::Float, escala::Grados} deriving Show

-- (d) 2. Definir una funci�n convert que dada una temperatura en grados Celsius la
-- convierta a grados Fahrenheit y viceversa. (Conversi�n de C a F: f = c * 9/5 + 32;
-- conversi�n de F a C: c = (f � 32) * 5/9.)

convert::Temperatura->Temperatura
convert t = if escala t == Celsius then Temperatura {valor=(valor t * 9/5 + 32), escala=Fahrenheit}
			else Temperatura {valor=(valor t - 32)*5/9, escala=Celsius}
			
{-	EJECUCION
	*T5P1TypesData> convert (Temperatura 30.5 Celsius)
	Temperatura {valor = 86.9, escala = Fahrenheit}
-}

-- (d) 3. Definir un tipo de datos para representar las �rdenes a los aparatos de a/a.
data Orden = ON | OFF deriving (Eq,Show)

-- (d) 4. Definir una funci�n action que dada una temperatura en cierta habitaci�n
-- determine la acci�n a realizar sobre el aparato de a/a de dicha habitaci�n. 
-- El controlador debe encender el aparato si la temperatura excede de 28�C. 

accion::Temperatura->Orden
accion t = if escala t == Celsius then accionAux t
		   else accionAux (convert t)

accionAux::Temperatura->Orden
accionAux t 
			|valor t <= 28 = OFF
			| otherwise = ON

	{- EJECUCION
		*T5P1TypesData> accion (Temperatura 30 Celsius)
		ON
		*T5P1TypesData> accion (Temperatura 30 Fahrenheit)
		OFF
	-}

-- ___________________________________________________________________________________________________________ --

{-  
(e) Definir un tipo moneda para representar euros y d�lares USA. Definir una funci�n que
		convierta entre ambas monedas sabiendo que el factor de conversi�n de euros a d�lares
		es 1.14.
-}


type Valor = Float
data Tipo = EURO | DOLAR deriving (Show,Eq)
data Moneda = Moneda Valor Tipo deriving (Show,Eq)
-- Si usamos este tipo de "constructores" vamos a necesitar un get para coger cada uno de los valores

m=Moneda 4 EURO

getValor::Moneda->Float
getValor (Moneda a _) = a

getTipo::Moneda->Tipo
getTipo (Moneda _ a) = a

	{- EJECUCION
		*T5P1TypesData> getTipo (Moneda 5 EURO)
		EURO
		*T5P1TypesData> getValor (Moneda 5 EURO)
		5.0
	-}

convertirMoney::Moneda->Moneda
convertirMoney mon
				|getTipo mon == EURO = (Moneda ((getValor mon)*1.14) DOLAR)
				|otherwise = (Moneda ((getValor mon)*1/1.14) EURO)

	{- EJECUCION
		*T5P1TypesData> convertirMoney (Moneda 1 DOLAR)
		Moneda 0.877193 EURO
		*T5P1TypesData> convertirMoney (Moneda 25 DOLAR)
		Moneda 21.929825 EURO
		*T5P1TypesData> convertirMoney (Moneda 1 EURO)
		Moneda 1.14 DOLAR
	-}

data Moneda' = Moneda' (Valor,Tipo) deriving (Show,Eq) 		-- Moneda' (7.56,DOLAR)

convertirMoneda'::Moneda'->Moneda'
convertirMoneda' m 
				|getTipo' m == EURO = Moneda' ((getValor' m)*1.14,DOLAR)
				|otherwise = Moneda' ((getValor' m)*1/1.14,EURO)
					 
getValor'::Moneda'->Float
getValor' (Moneda'(v,_)) = v

getTipo'::Moneda'->Tipo
getTipo' (Moneda'(_,t)) = t


data Moneda'' = Moneda'' {val::Valor, tipo::Tipo} deriving (Show,Eq) 	--Moneda''{val=5.89, tip=EURO}
convertirMoneda''::Moneda''->Moneda''
convertirMoneda'' m
					| tipo m == EURO = Moneda'' {val= val m*1.14, tipo= DOLAR}
					| otherwise = Moneda'' {val= val m*1/1.14, tipo= EURO}

					

-- ___________________________________________________________________________________________________________ --					
{-
f) Dada el siguiente tipo de datos recursivo que representa expresiones aritm�ticas:
	data Expr = Valor Integer
			|Expr :+: Expr
			|Expr :-: Expr
			|Expr :*: Expr deriving Show
	e.1) Se pide una funci�n para calcular el valor de una expresi�n.
	e.2) Se pide una funci�n para calcular el n�mero de constantes de una expresi�n.

-}
