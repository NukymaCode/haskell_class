
module Haskell_EjercicioAEROPUERTO where

{- 
La Agencia Europea de Seguridad Aérea desea crear un programa en Haskell que les permita unificar la
información de todos los aeropuertos que gestionan y, así, poder realizar comparaciones entre ellos. La
información que quieren almacenar de cada aeropuerto es la siguiente:
	• Nombre del aeropuerto.
	• Número de pistas que tiene el aeropuerto.
	• Tráfico aéreo: se representa como el número de operaciones aéreas (vuelos).
	• Número de accidentes que se han producido en el aeropuerto.
	• Compañías aéreas que operan en el aeropuerto: será una lista con los nombres de las compañías
	aéreas.

	Se pide:
	a) Definir el tipo de dato adecuado para gestionar aeropuertos.
	
	b)  Implementar las clases necesarias para poder comparar aeropuertos y poder mostrar
	su información por pantalla. Dos aeropuertos serán iguales si tienen el mismo número de pistas, si
	tiene el mismo tráfico aéreo (mismo número de operaciones) y, por último, si en el aeropuerto
	operan el mismo número de compañías aéreas (aunque sean diferentes compañías). Por último,
	dado un aeropuerto, su visualización por pantalla debería ser similar a la siguiente:
	Aeropuerto: Barajas; Pistas: 6; Vuelos: 200; Companias: 2; Accidentes: 2 
	
	c)  Implementar una función en Haskell que dado un aeropuerto a y una lista de
	aeropuertos, devuelva una lista con los aeropuertos que son iguales que a y que tiene mayor número
	de accidentes que a.
-}



type Compania = String

data Aeropuerto = Aeropuerto {
								nombre::String,
								pistas::Int,
								trafico::Int,
								accidentes::Int,
								companias::[Compania]
							  } 
							  
-- COMPARAR AEROPUERTOS
-- Iguales si:
				-- mismo número de pistas
				-- mismo número de operaciones
				-- mismo número de compañías aéreas 
instance Eq Aeropuerto where
	Aeropuerto nombre1 pistas1 trafico1 accidentes1 companias1 == Aeropuerto nombre2 pistas2 trafico2 accidentes2 companias2 = 
			(pistas1==pistas2) && (trafico1==trafico2) && (length(companias1)==length(companias2)) 
			

a1::Aeropuerto
a1 = Aeropuerto {
		nombre="Barajas",
		pistas=5,
		trafico=5,
		accidentes=5,
		companias=["Iberia","Caca"]
		}
		
a2::Aeropuerto
a2 = Aeropuerto {
		nombre="Heathrow",
		pistas=5,
		trafico=5,
		accidentes=0,
		companias=["Iberia","Mierda"]
		}
		
a3::Aeropuerto
a3 = Aeropuerto {
		nombre="Holi",
		pistas=4,
		trafico=3,
		accidentes=2,
		companias=["Iberia","Mierda","CacaMierda"]
		}

-- MOSTRAR INFO POR PANTALLA
-- 	Aeropuerto: Barajas; Pistas: 6; Vuelos: 200; Companias: 2; Accidentes: 2 
instance Show Aeropuerto where
		show (Aeropuerto n p t a c) = "Aeropuerto:"++show n++"; Pistas:"++show p++";"


{-
	c) Implementar una función en Haskell que dado un aeropuerto a y una lista de
	aeropuertos, devuelva una lista con los aeropuertos que son iguales que a y que tiene mayor número
	de accidentes que a.
-}

aeropuertosIguales::Aeropuerto ->[Aeropuerto]->[Aeropuerto]
aeropuertosIguales aero listaAero = foldr(\ a acum -> if (aero==a) && (accidentes a > accidentes aero) then acum++[a]
														else acum ) [] listaAero