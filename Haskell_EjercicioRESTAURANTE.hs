
module Haskell_EjercicioRESTAURANTE where
-- ___________________________________________________________________________________________________________ --

-- EJERCICIO RESTAURANTE
-- ___________________________________________________________________________________________________________ --

{-
	En un restaurante se desea gestionar las mesas libres y ocupadas de forma que se
	puedan asignar mesas rápidamente según llegan los comensales. De cada mesa se
	necesita conocer el número de mesa y su capacidad. Se pide implementar un tipo de
	datos Restaurante, donde se puedan saber las mesas libres y las mesas ocupadas. La
	representación por pantalla del tipo ocupación debe ser la siguiente:
	
		Libres:
		[Mesa 1 -> Capacidad:10]
		Ocupadas:
		[Mesa 2 -> Capacidad:20]
		
	Se pide además implementar las siguientes funciones:
		• insertarMesaLibre: esta función, dada un restaurante y una mesa, inserta la
		mesa en la lista de mesas libres de forma ordenada, con las mesas con menor
		capacidad primero.
		• ocuparMesa: dada un restaurante y un número de comensales, esta función
		devuelve una nueva ocupación, donde a los comensales se les ha asignado la
		mesa libre más pequeña en la que caben todos, y esta mesa ha sido añadida a la
		lista de mesas ocupadas y eliminada de la lista de mesas libres.
-}

-- PARTE 1 DEL EJERCICIO RESTAURANTE
-- ___________________________________________________________________________________________________________ --
-- 1) Primero me creo los tipos de datos que voy a necesitar:
type Capacidad = Integer
data Mesa = Mesa {numero::Integer, capacidad::Capacidad} -- deriving Show
data Restaurante = Restaurante {mesasLibres::[Mesa], mesasOcupadas::[Mesa]}

-- Ahora me creo una "variable" mesa para poder probar su representacion
mi_mesa::Mesa
mi_mesa = Mesa {numero=10, capacidad=10}
-- Sin hacerle nada, al sacar por pantalla se muestra así (tendríamos que ponerle el deriving Show a Mesa)
	-- *T5P3ClassESOther> mi_mesa
	-- Mesa {numero = 10, capacidad = 10}
-- Pero queremos que se muestre así:
	-- Mesa 10 -> Capacidad:10

-- 2) Para que se muestre por pantalla como nos piden, tenemos que crear una instacion de Show:
instance Show Mesa where
	show (Mesa num ocu) = "Mesa " ++ show num ++ " -> Capacidad:" ++ show ocu ++" "
-- Ahora se ve así:
	-- *T5P3ClassESOther> mi_mesa
	-- Mesa 10 -> Capacidad:10

-- Ahora me creo un par de "variables" de listas de mesas
libres::[Mesa]
libres = [
			(Mesa {numero=1, capacidad=5}),
			(Mesa {numero=3, capacidad=7}),
			(Mesa {numero=5, capacidad=5})
		]		
ocupadas::[Mesa]
ocupadas = [
				(Mesa {numero=2, capacidad=4}),
				(Mesa {numero=4, capacidad=8})
			]
-- Si ejecuto, así si muestran:
	-- *T5P3ClassESOther> libres
	-- [Mesa 1 -> Capacidad:5 ,Mesa 3 -> Capacidad:7 ,Mesa 5 -> Capacidad:5 ]

-- 3) Vamos a crear una instacia de Show para representar 
instance Show Restaurante where
	show (Restaurante listalib listaocu) = "Libres:\n" ++ show listalib ++ "\nOcupadas:\n" ++ show listaocu

-- Vamos a crear una "variable" restaurante para probar como se muestra por pantalla un tipo Restaurante
mi_restaurante::Restaurante
mi_restaurante = Restaurante {mesasLibres=libres, mesasOcupadas=ocupadas}
{- Así es como se ve:
	*T5P3ClassESOther> mi_restaurante
	Libres:
	[Mesa 1 -> Capacidad:5 ,Mesa 3 -> Capacidad:7 ,Mesa 5 -> Capacidad:5 ]
	Ocupadas:
	[Mesa 2 -> Capacidad:4 ,Mesa 4 -> Capacidad:8 ]
-}

-- ___________________________________________________________________________________________________________ --
-- PARTE 2 DEL EJERCICIO RESTAURANTE - insertarMesaLibre
-- ___________________________________________________________________________________________________________ --
{- insertarMesaLibre: 
		esta función, dada un restaurante y una mesa, inserta la
		mesa en la lista de mesas libres de forma ordenada, con las mesas con menor
		capacidad primero.
-}
-- Para poder insertar de forma ordenada, primero tengo que poder comparar mesas.
-- Cremos una instancia de ORDEN
instance Ord Mesa where
	Mesa num1 cap1 >= Mesa num2 cap2 = cap1 >= cap2
	Mesa num1 cap1 <= Mesa num2 cap2 = cap1 <= cap2
	Mesa num1 cap1 > Mesa num2 cap2 = cap1 > cap2
	Mesa num1 cap1 < Mesa num2 cap2 = cap1 < cap2
	
instance Eq Mesa where
	Mesa num1 cap1 == Mesa num2 cap2 = cap1 == cap2
	
insertarMesaLibre::Restaurante->Mesa->Restaurante
insertarMesaLibre re me = Restaurante {mesasLibres=(addMesaOrdenada (mesasLibres re) me), mesasOcupadas= mesasOcupadas re}

-- Con este método auxiliar inserto de forma ordenada
addMesaOrdenada::[Mesa]->Mesa->[Mesa]
addMesaOrdenada [] mesa = [mesa]
addMesaOrdenada (m1:mesas) m2 
				| m1 >= m2 = m2:m1:mesas 
				| otherwise = m1:addMesaOrdenada mesas m2
					-- Si la primera mesa que tenemos en la lista tiene más capacidad que la que vamos a meter, pues la nueva debe ir delante
					-- Si no, metemos la mesa cabecer a de la lista y volvemos a llamar a la funcion
					
	{- EJECUCION
		*T5P3ClassESOther> insertarMesaLibre mi_restaurante mi_mesa
		Libres:
		[Mesa 1 -> Capacidad:5 ,Mesa 3 -> Capacidad:7 ,Mesa 5 -> Capacidad:5 ,Mesa 10 -> Capacidad:10 ]
		Ocupadas:
		[Mesa 2 -> Capacidad:4 ,Mesa 4 -> Capacidad:8 ]
	-}					


-- ___________________________________________________________________________________________________________ --	
-- PARTE 3 DEL EJERCICIO RESTAURANTE - OcuparMesa
-- ___________________________________________________________________________________________________________ --
{-ocuparMesa:
 	dada un restaurante y un número de comensales, esta función devuelve una nueva ocupación, 
 	donde a los comensales se les ha asignado la mesa libre más pequeña en la que caben todos, 
 	y esta mesa ha sido añadida a la lista de mesas ocupadas y eliminada de la lista de mesas libres.
-}		
type Comensales = Integer

--ocuparMesa::Restaurante->Comensales->Restaurante
--ocuparMesa re co = ocuparMesaAux re co []		

-- data Restaurante = Restaurante {mesasLibres::[Mesa], mesasOcupadas::[Mesa]}


{-
ocuparMesaAux::Restaurante->Comensales->[Mesa]->Restaurante
ocuparMesaAux r numcomen mesasPequenas
					|capacidad (m:(mesasLibres r)) >= numcomen = Restaurante{mesasLibres=(mesasLibres r), mesasOcupadas=mesasOcupadas r ++[m]}
					| otherwise 
-}

