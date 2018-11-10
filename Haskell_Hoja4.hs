
module Haskell_Hoja4 where

import Data.Char
import Data.List

-- ___________________________________________________________________________________________________________ --
-- EJERCICIO E - ESTUDIANTES Y ASOCIACIONES
-- ___________________________________________________________________________________________________________ --

{- ANA
	e) Se quiere poder mostrar por pantalla los datos de los estudiantes 
	matriculados en una universidad que pertenezcan a alguna de las asociaciones 
	de esta (culturales, deportivas, de representacion estudiantil, etc.). 
	Para ello se deberan crear nuevos tipos de datos que representen: 
		• Estudiante, de cada uno se debe disponer del nombre y titulacion 
		• Titulacion, que pueden ser tres: Grado II, Grado II_ADE, Grado ADE 
		• Lista de estudiantes matriculados 
		• Lista de estudiantes que pertenecen a asociaciones 
	 
 	Un ejemplo de aplicacion de la funcion que se pide podria ser: 
 
	> mostrarAlumnosAsociaciones(listaMatriculados,listaAsociaciones)  
	"(Carlos Calle,GradoADE_II)(Irene Plaza,GradoADE)" 
 
	Donde Carlos Calle e Irene Plaza son los únicos estudiantes matriculados que 
	pertenecen a algún tipo de asociacion en la universidad.

-}

-- Lo primero es crear los nuevos tipos de datos que necesitamos
type Nombre = String

data Titulacion = Grado_II | Grado_II_ADE | Grado_ADE deriving (Eq,Show)
data Estudiante = Estudiante {nombre::Nombre, titulacion::Titulacion} deriving (Eq,Show)
data Asociacion = Asociacion {asociacion::Nombre} deriving (Eq)

-- Necesitamos un nuevo tipo de dato que sea una lista de estudiantes para poder mostrarla después.
-- DATA y NEWTYPE hacen lo mismo.
-- data ListaEstudiantes = ListaEstu [Estudiante]

newtype ListaEstudiantes = ListaEstu {estudiantes::[Estudiante]} deriving (Eq,Show)

-- Nos creamos una lista de estudiantes matriculados
listaEstudiantesMatriculados :: ListaEstudiantes
listaEstudiantesMatriculados = ListaEstu
	[ 	
		(Estudiante {nombre ="Sonia Negro", titulacion = Grado_II_ADE}),
		(Estudiante {nombre ="Pepe Garcia", titulacion = Grado_ADE}),
		(Estudiante {nombre ="Alba Toro", titulacion = Grado_II}),
		(Estudiante {nombre ="Sonia Blanco", titulacion = Grado_II})
	]
	
-- Nos creamos una lista con estudiantes matriculados que pertenecen a alguna asociacion	
listaEstudianteAsociaciones :: ListaEstudiantes
listaEstudianteAsociaciones = ListaEstu
	[ 
		(Estudiante {nombre ="Pepe Garcia", titulacion = Grado_ADE}),
		(Estudiante {nombre ="Alba Toro", titulacion = Grado_II})
	]
	
-- Lo que hace este método es mostrar lo que hay en común en las dos lista que le hemos pasado.
-- Coge lo común de ambas listas y lo deja en otra lista
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html

mostrarAlumnosAsociaciones::ListaEstudiantes->ListaEstudiantes->ListaEstudiantes
mostrarAlumnosAsociaciones (ListaEstu lista1) (ListaEstu lista2) = ListaEstu (intersect lista1 lista2)
	{- EJECUCION
		*T5P3ClassESOther> mostrarAlumnosAsociaciones listaEstudiantesMatriculados listaEstudianteAsociaciones
		ListaEstu {estudiantes = [Estudiante {nombre = "Pepe Garcia", titulacion = Grado_ADE},Estudiante {nombre = "Alba Toro", titulacion = Grado_II}]}
	-}

-- ___________________________________________________________________________________________________________ --
-- EJERCICIO F - FECHAS
-- ___________________________________________________________________________________________________________ --

{- ANA
	(f) Se quiere poder representar una fecha de la siguiente forma: dd/mm/aaaa, 
	para ello se debera crear un nuevo tipo de datos en Haskell. 
	Por ejemplo, si se crea un nuevo tipo de datos cuyo constructor de datos 
	es Fecha, en el interprete al poner fechas concretas nos devolveria la 
	representacion de la fecha que hayamos definido: 
		> Fecha 10 10 2013 
		10/10/2013  
		> Fecha 24 12 2012
		24/12/2012 
-}
 -- Constructor de datos es lo que va en la segunda parte del igual:
 --  data (constructor de tipo) = (constructores de datos)
 
data FechaTipo = Fecha { dia::Int, mes::Int, anio::Int} deriving Eq

-- Para incluir un tipo en una clase se realiza una INSTANCIA 
-- En este caso para poder mostrar la fecha, tengo que crear una instancia de SHOW 
instance Show FechaTipo where
	show (Fecha d m a) = show d++"/"++(show m)++"/"++(show a)
	{- EJECUCIÓN
		*T5P3ClassESOther> Fecha 10 10 2018
		10/10/2018
		*T5P3ClassESOther> Fecha {dia=10,mes=10,anio=2018}
		10/10/2018
	-}
 
-- ___________________________________________________________________________________________________________ --
-- EJERCICIO G - MISMA FECHA
-- ___________________________________________________________________________________________________________ --
{- ANA
	g) Teniendo en cuenta el nuevo tipo de datos Fecha definido anteriormente, se pide una
	función que sea capaz de comparar dos fechas. Ejemplos de aplicación de la función serían:
	> mismaFecha (Fecha 10 10 2013) (Fecha 10 10 2013)
	True
	> mismaFecha (Fecha 10 11 2013) (Fecha 10 10 2013)
	False
-}

compararFechas::FechaTipo->FechaTipo->Bool
compararFechas f1 f2 = if (dia f1 == dia f2) then 
							if mes f1==mes f2 then 
								if anio f1==anio f2 then True
								else False
							else False
						else False
						


{- EJECUCION
	*T5P3ClassESOther> compararFechas (Fecha 10 01 2000) (Fecha 10 01 2000)
	True
	*T5P3ClassESOther> compararFechas (Fecha 11 01 2000) (Fecha 10 01 2000)
	False
	*T5P3ClassESOther> compararFechas (Fecha 10 01 2000) (Fecha 10 03 2000)
	False
	*T5P3ClassESOther> compararFechas (Fecha 11 01 2500) (Fecha 10 01 2000)
	False
	*T5P3ClassESOther> compararFechas (Fecha 11 01 2500) (Fecha 10 05 2000)
	False
-}

-- ___________________________________________________________________________________________________________ --
-- EJERCICIO I - COLECCION-PILAS-COLAS
-- ___________________________________________________________________________________________________________ --
{- ANA + CLASE
	i) Se pide crear una nueva clase de tipos, llamada Coleccion, para representar colecciones
	de datos de cualquier tipo, donde los tipos pertenecientes a esta clase tendrán el siguiente
	comportamiento:
		esVacia: función para saber si la colección está vacía.
		insertar: insertará un nuevo elemento en la colección.
		primero: devolverá el primer elemento de la colección.
		eliminar: eliminará un elemento de la colección.
		size: devolverá el número de elementos de la colección.
		
	Algunas de las funciones anteriores variarán su implementación en función del tipo de
	colección particular que sea instancia de la clase Coleccion. Por ello, se pide crear dos
	instancias diferentes de esta clase para los dos nuevos tipos de datos que se presentan a
	continuación:
	
		data Pila a = Pil [a] deriving Show
		data Cola a = Col [a] deriving Show
	
	El primero de ellos representa una estructura de datos LIFO con elementos de tipo a. 
	El segundo representa una estructura de datos FIFO de elementos de tipo a.
-}

class Coleccion c where
  	esVacia :: c a -> Bool  			-- Una colección de elementos a 
  	insertar :: a -> c a -> c a 		-- Un elemento de tipo a para insertarlo en una colección de elementos de a
  	primero :: c a -> a
  	eliminar :: c a -> c a 		
	size :: c a -> Int	
-- Lo de la clase es como decir que cuando tengamos una Coleccion vamos a poder aplicarle estos métodos. Lo "dejamos como indicado".
-- En plan, cuando tengas una Coleccion de "avispas" vas a poder decir cual es el primero y el tamaño y ...

-- La implementación de estos métodos ha de hacerse con instance. Ahí es dónde vas a decir como se mira el primero de una colección, etc, 
-- dependiendo de la colección de qué elementos.
	
-- Algunas de las funciones anteriores se implementan de dos formas distintas dependiendo del tipo de colección
-- Creamos dos tipos de datos. Pilas de elementos a y Colas de elementos a
-- PARA INCLUIR UN TIPO EN UNA CLASE HA DE RELIZARSE UNA INSTANCIA

data Pila a = Pila [a] deriving (Show)
mi_pila::Pila Integer
mi_pila = Pila [1,2,3,4,5]

instance Coleccion Pila where
	esVacia (Pila p) = length p == 0
	insertar a (Pila p) = Pila (p ++ [a]) 		-- Se inserta por la derecha (al final)
	primero (Pila p) = last p					-- Se mira por la derecha (el último)
	eliminar (Pila p) = Pila (init p)			-- Se elimina por la derecha (el último elemento de la lista)
												-- init: Return all the elements of a list except the last one. The list must be non-empty.
	size (Pila p) = length p
	
	
data Cola a = Cola [a] deriving (Show)
mi_cola::Cola Char
mi_cola = Cola "HOLA"

instance Coleccion Cola where
	esVacia (Cola c) = length c == 0
	insertar a (Cola c) = Cola (c ++ [a]) 		-- Se inserta por la derecha (al final)
	primero (Cola c) = head c					-- Se mira por la izquierda (el primero)
	eliminar (Cola c) =	Cola (tail c)			-- Se elimina por la izquierda (el primer elemento de la lista) 
												-- tail: Extract the elements after the head of a list, which must be non-empty.
	size (Cola c) = length c
	
	
{- EJECUCION PILA

	*T5P3ClassOther> mi_pila
	Pila [1,2,3,4,5]
	*T5P3ClassOther> insertar 9 mi_pila
	Pila [1,2,3,4,5,9]
	*T5P3ClassOther> primero mi_pila
	5
	*T5P3ClassOther> eliminar mi_pila
	Pila [1,2,3,4]
	*T5P3ClassOther> size mi_pila
	5

-}	
	
{- EJECUCION COLA

	*T5P3ClassOther> size mi_cola
	4
	*T5P3ClassOther> primero mi_cola
	'H'
	*T5P3ClassOther> eliminar mi_cola
	Cola "OLA"
	*T5P3ClassOther> mi_cola
	Cola "HOLA"
	
	*T5P3ClassOther> insertar "S" mi_cola
	
	*T5P3ClassOther> <interactive>:8:14:
	    Couldn't match type `Char' with `[Char]'
	    Expected type: Cola [Char]
	      Actual type: Cola Char
	    In the second argument of `insertar', namely `mi_cola'
	    In the expression: insertar "S" mi_cola
-}
	
	
	
-- ___________________________________________________________________________________________________________ --
-- *********************************************************************************************************** --
-- EJERCICIOS QUE FALTAN POR HACER
-- ___________________________________________________________________________________________________________ --
-- *********************************************************************************************************** --


-- ___________________________________________________________________________________________________________ --
-- EJERCICIO B - DIVISIONES
-- ___________________________________________________________________________________________________________ --
{- ANA
	b) Se pide implementar una función que dada un número (de cualquier tipo que soporte la
	operación de división) y una lista de números del mismo tipo, divida a ese número por cada
	uno de los elementos contenidos en la lista y devuelva una lista con el resultado.
	
	Ejemplos de aplicación de la función son:
		> divisiones 5 [1,2,3]
		[Just 5,Just 2,Just 1]
		> divisiones 5 [1,2,3,0,9,10]
		[Just 5,Just 2,Just 1,Nothing,Just 0,Just 0]
-}


-- ___________________________________________________________________________________________________________ --
-- EJERCICIO A - QUICKSORT
-- ___________________________________________________________________________________________________________ --
{- ANA
	a) Se quiere ordenar los elementos de una lista (cuyos elementos son comparables) mediante el algoritmo del quicksort.
-}


-- ___________________________________________________________________________________________________________ --
-- EJERCICIO H - FECHA QUICKSORT
-- ___________________________________________________________________________________________________________ --
{- ANA
	h) Teniendo en cuenta la definición de la función qs del apartado (b) de este listado de
	ejercicios, se pide ordenar una lista de fechas mediante quicksort. Ejemplos de aplicación
	de la función serían:
		> qs [(Fecha 10 10 2013), (Fecha 24 12 2012), (Fecha 10 09 2013), (Fecha 12 12 2013)]
		[24/12/2012,10/9/2013,10/10/2013,12/12/2013]
-}

