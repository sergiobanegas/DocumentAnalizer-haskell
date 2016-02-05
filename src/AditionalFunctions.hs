module AditionalFunctions where
import Data.Char

--Imprime una lista
printList :: [String] -> IO()
printList [] = putStr ""
printList (x:xs) = do
					putStr "- "
					putStr x	
					putStr "\n"
					printList xs
					
--Pone en mayúsculas la primera letra de un string				
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = [toUpper x]++xs		

--Comprueba si un string contiene un paréntesis
containsParenthesis :: String -> Bool
containsParenthesis [] = False
containsParenthesis (x:xs) = if (x=='(')||(x==')') then
									True
							 else
							 	containsParenthesis xs

--Comprueba si una lista de strings contiene paréntesis
hasParenthesis :: [String] -> Bool
hasParenthesis [] = False
hasParenthesis (x:xs) = if (containsParenthesis x) then
							True
						else 
							hasParenthesis xs

--Comprueba si un listado de palabras contienen palabras que no se deben de contar
hasInvalidWord :: [String] -> Bool
hasInvalidWord [] = False
hasInvalidWord (x:xs) = if (x=="of")||(x=="and") then
							True
						else
							hasInvalidWord xs	
							
--Devuelve el número de palabras inválidas que tiene un conjunto de palabras				
invalidWords :: [String] -> Int
invalidWords [] = 0
invalidWords (x:xs) = if (x=="of")||(x=="for")||(x=="and") then
								1+(invalidWords xs)
						  else
						  		invalidWords xs
							
--Dado un listado de palaras elimina las comillas							
removeQuotes :: [String] -> [String]
removeQuotes [] = []
removeQuotes (x:xs) = if ((head x)=='“') then
							[(tail x)]++(removeQuotes xs)	
					  else if ((head (reverse x))=='”') then
					  		[(reverse (tail (reverse x)))]++(removeQuotes xs)
					  else 
							[x]++(removeQuotes xs)
							
--Dada una lista de strings borra los elementos en blanco							
deleteEmptys :: [String] -> [String]
deleteEmptys [] = []
deleteEmptys (x:xs) = if (x=="") then
					  	deleteEmptys xs
					  else
					  	[x]++(deleteEmptys xs)
					  	
--Devuelve true si dada una palabra todos las letras están en mayúscula					  	
isAllUpper :: [String] -> Bool
isAllUpper (x:[]) = if (isUpper (head x)) then
					True
				  else
				  	False
isAllUpper (x:xs)= if (isUpper (head x)) then
					True && (isAllUpper xs)
				else	
					False
					
allUpper :: String -> String
allUpper [] = ""
allUpper (x:xs) = [(toUpper x)]++(allUpper xs)														