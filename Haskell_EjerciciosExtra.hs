
module Haskell_EjerciciosExtra where
import Data.Char
{- Recopilación de ejercicios propuestos en clase-}

--------------------------------------------------------------------------------------------------------------------------------
-- RECIBE UNA PALABRA Y TE DEVUELVE UNA LISTA CON LA VOCALES Y OTRA CON LAS CONSONANTES
--------------------------------------------------------------------------------------------------------------------------------
separarVoCo::[Char]->([Char],[Char])
separarVoCo palabra =   foldr(\letra (vocal,consonante)-> 
                            if (elem letra vocal) then (letra:vocal,consonante)
                            else (vocal,letra:consonante)
                            ) ([],[]) palabra
                            
vocal = ['a','e','i','o','u','A','E','I','O','U']

--------------------------------------------------------------------------------------------------------------------------------
-- INTERCAMBIAR MAYUSCULAS POR MINUSCULAS
-- cambioMayMin ["Madrid", "Barcelona","Bilbao"]  ->  ["mADRID","bARCELONA","bILBAO"]
--------------------------------------------------------------------------------------------------------------------------------
cambioMayMin::[[Char]]->[[Char]]
cambioMayMin lista = map (foldl(\acum c -> if isUpper c then acum++[toLower c] else acum++[toUpper c]) []) lista

--------------------------------------------------------------------------------------------------------------------------------
-- SEPARAR LISTA -- 
-- Devolvemos en una lista los elementos que aparecen 1 vez y en otra lista los que aparecen m�s de una vez peeeero s�lo ha de aparece una vez.
-- apariciones [3,7,4,3,2,7,3,1,-1,2] -> ([4,1,-1],[3,7,2])
-----------------------------------------------------------------------------------------------------------------------------------------------
-- Como proceder?
-- CASO BASE: La lista a separa es vacia, con lo que devolver�amos una tupla de listas vac�as ([UNICOS], [REPETIDOS]) ([],[])
-- CASO GENERAL: Cojo el primer elemento de la lista (foldl). 
				--Miro si aparece en la lista de REPETIDOS, no se hace nada y devolvemos lo que ten�amos
				-- si no: Miro si aparece en la lista de UNICOS, si es asi, he de borrar el elemento de la de UNICOS y añadirlo en la de repetidos
				--        si no: añadimos el elemento en la lista de unicos
			
separarLista::[Int]->([Int],[Int])
separarLista lista = foldl (\ (unicos,repes)  x ->	if pertenece repes x then (unicos,repes)
														else if (pertenece unicos x) then (borrar x unicos, repes ++[x])
                                                        else (unicos++[x],repes) ) ([ ],[ ]) lista   
                                                                   
-- Funcion pertenece
pertenece::[Int]->Int->Bool
pertenece []_= False
pertenece (x:xs) num = (num == x) || (pertenece xs num)

-- Funcion borrar un elemento de una lista
borrar::Int->[Int]->[Int]
borrar _ [] = []
borrar x (y:ys) = if x==y then ys else y:(borrar x ys)


--------------------------------------------------------------------------------------------------------------------------------
-- ELIMINAR LOS ELEMENTOS QUE EST�N EN LA POSICI�N PAR DE LA LISTA
-- Devolvemos la lista sin los elementos en la posici�n par. Me tengo que quedar con los elementos de las posiciones impares
--------------------------------------------------------------------------------------------------------------------------------
eliminarPosicionPar::[Int]->[Int]
eliminarPosicionPar lista = resultado 
							where (resultado,posicion) = posicionParAux lista
								
		-- eliminarPosicionPar nos devuelve RESULTADO. Resultado sale de (resultado, posicion) 
		-- que es el output de la funcion posicionParAux al pasarle la lista original
		
		-- Nos da una lista y vamos a devolver una dupla de una lista y una posici�n
		-- CASO BASE: la lista que nos llega est� vac�a, por lo que devolver�amos una lista vac�a y la posici�n 0 ([],0)
		-- Voy a empezar a leer la lista por la izquierda(foldl)
		-- En la funci�n LAMBDA vamos a llevar la acumulaci�n y la posici�n. 
		-- ACUMULACI�N es la lista donde vamos a ir metiendo los elementos. POSICION es donde vamos guardando la posicion
		-- Si la posicion es par, devolvemos la lista tal cual y aumentamos la posicion
		-- Si la posici�n es impar, metemos el elemento en la lista y aumentamos la posicion

posicionParAux::[a]->([a],Int)
posicionParAux lista = foldl(\(acum, pos) x -> if even pos then (acum,pos+1) else (acum++[x],pos+1) ) ([],0) lista
												
                        -- ([],0) Voy a ir metiendo en la lista vac�a los elementos de las posciones impares
                        -- y el 0 es la posici�n original de la que parto
                        -- Esta funci�n devuelve una tupla, pero s�lo nos interesa el primero, por eso filtramos con el WHERE de la funci�n eliminarPosicionPar
