
module Haskell_Hoja5 where
import Data.Char

-- *** ENTRADA / SALDIDA ***

{-
1. Función que dado un listado de nombres lo muestre por pantalla en forma de tabla.
	> escribeTabla ["pepe","caramelo","lluvia"]
	1: pepe
	2: caramelo
	3: lluvia
-}
escribeTabla::[String]->IO( )
escribeTabla s =escribeTablaAux s 1 

escribeTablaAux::[String]->Int->IO( )
escribeTablaAux [ ] cont =return()
escribeTablaAux (x:xs) cont = do
				 print (show cont++": "++x)
				 escribeTablaAux xs (cont+1)


{-
2. Definir una función que sea capaz de leer dos líneas de la entrada estándar y las
compare, escribiendo una cadena por pantalla que indique si son iguales o no.
> comparaCadenas
Introduce la primera linea: linea1
Introduce la segunda linea: hola
Las cadenas son diferentes
-}
compararCadenas::String->String->Bool
compararCadenas [ ] [ ]= True
compararCadenas [ ] _ = False
compararCadenas _ [ ] = False
compararCadenas (s1:xs1) (s2:xs2) = (s1==s2)&&(compararCadenas xs1 xs2)

comparaCadenas::IO( )
comparaCadenas = do
                   s1<-getLine;
                   s2<-getLine;
                   let result=compararCadenas s1 s2
                   if result then print "La cadenas son iguales" else print "Las cadenas son diferentes"

{-
3. Definir una función que lea el contenido de un fichero de texto, lo procese
invirtiendo todo el contenido y lo escriba de nuevo sobre el mismo fichero de
entrada.
-}
invertirFichero::IO()
invertirFichero = do
		    print "Nombre del archivo origen"
		    archivoO<-getLine
		    print "Nombre del archivo destino"
		    archivoD<-getLine
		    contenido<-readFile archivoO
		    let invertido = foldr (\c acum->acum++[c] ) [ ]contenido
		    writeFile archivoD invertido

{-
4. Definir una función que sea capaz de ir leyendo líneas de la entrada estándar y las
va imprimiendo junto con el número de caracteres que tienen. Se irá ejecutando
mientras no se encuentra una línea vacía. Un ejemplo del resultado de la ejecución
de la función puede ser:
> leerLineas
Introduce una linea: hola
La linea tiene 4 caracteres
Introduce una linea: casita
La linea tiene 6 caracteres
Introduce una linea:
-}
-- La estructura "else do" con la llamada al final de nuevo a la función, crea una especia de bucle while
contarLineas::IO()
contarLineas=do
		print "Introduce una linea: "
		linea<-getLine
		let longitud = length linea
		if (longitud==0) 
			then  
			  print "La linea tiene: "
			else  do
			      print ("La linea tiene: " ++show longitud	)
			      contarLineas


{-
5. Definir una función que sea capaz de copiar el contenido
-}


-- ___________________________________________________________________________________________________________ --
-- EJEMPLOS TEORÍA
-- ___________________________________________________________________________________________________________ --

{- 
Se lee el contenido de un fichero de entrada y se convierte a mayúsculas 
escribiendo el de entrada y se convierte a mayúsculas escribiendo el resultado en un fichero de salida. 
-}
misecuencia::IO()
misecuencia = do
			-- Muestro mensaje y pido el nombre del fichero de entrada
			putStrLn "Fichero de entrada: "
			fentrada <- getLine
			-- Muestro mensaje y pido el nombre del fichero de salida
			putStrLn "Fichero de salida: "
			fsalida <- getLine
			-- Leo línea por línea el fichero de entrada
			cadena <- readFile fentrada
			writeFile fsalida (map toUpper cadena)
			putStr "CONVERSION REALIZADA"


-- Usamos el DO para tener una secuencia de acciones y/o para capturar y dar nombre a valores decueltos por acciones IO
invertir::[a]->[a]
invertir lista = foldr(\x acumulador -> acumulador++[x]) [] lista

main::IO()
main = do
		print "Le quieres dar la vuelta a: "
		linea <- getLine
		putStrLn (invertir linea)
		