
module Haskell_EjercicioPISO where

{-
Una de las principales preocupaciones de los estudiantes que deciden venir de otras regiones a estudiar a la URJC 
es encontrar un alojamiento para poder pasar el curso. La Universidad, en su afán de facilitar dicha tarea a sus alumnos, 
ha decidido crear un repositorio de pisos de alquiler. De dichos pisos la información que maneja la URJC es: 
	dirección, distancia respecto a la Universidad, precio del alquiler mensual y capacidad. 

En cuanto a la capacidad, los mismos se caracterizan por el número de habitaciones individuales y dobles de que dispone cada piso.

	Define un tipo de dato adecuado para gestionar este tipo de alojamiento universitario. (0.5 puntos)
	
	Implementa todo lo que consideres necesario para poder comparar pisos. 
	Un piso es igual a otro si tiene la misma ratio precio/persona. 
	Un piso es mejor que otro si su precio por persona es más barato. 
	En caso de tener igual esta ratio, será mejor aquel que se encuentre más cerca de la Uni (2 puntos)
	
	Implementa todo lo necesario para que la información de un piso salga 
	por pantalla de una manera apropiada, dando todos los datos relativos a sus 
	características de una manera apropiada para el alumno que quiera consultar la misma (1.5 puntos)
-}

type Individual = Int
type Doble = Int
type Capacidad = (Individual, Doble)

data Piso = Piso {
				direccion::String,
				distancia::Int,
				precio::Int,
				capacidad::Capacidad
				} 
				
numeroPersonas::Capacidad->Int
numeroPersonas (x,y) = x+(y*2)

ratio::Piso->Int
ratio (Piso dir dis pre cap) = pre `div` numeroPersonas(cap)

instance Eq Piso where
	Piso a b c d == Piso e f g h = ratio(Piso a b c d) == ratio(Piso e f g h)

-- He considerado la ordenacion con los ratios y el numero de personas
-- Un piso es más grande que otro si es mejor (es decir si tiene ratio menor)
instance Ord Piso where
	Piso a b c d > Piso e f g h = (ratio(Piso a b c d) < ratio(Piso e f g h)) 
	Piso a b c d <= Piso e f g h = (ratio(Piso a b c d) >= ratio(Piso e f g h)) 

instance Show Piso where
	show (Piso dir dis pre (ind,dob)) = "El piso en "++show dir++" esta a "++show dis++" metros, cuesta "++show pre++" euros, tiene "++show ind++" habitacion/es individual y "++show dob++" habitacion/es doble"


mi_piso::Piso
mi_piso = Piso {
				direccion="C/SinNombre 25", 
				distancia=500,
				precio=700, 
				capacidad=(2,1)
				}
				
mi_piso'::Piso
mi_piso' = Piso {
				direccion="C/SinNombre 25", 
				distancia=1000,
				precio=100, 
				capacidad=(2,5)
				}
				