module Exercises where

import Article
import GetArticles
import Acronym
import AditionalFunctions
import OrthographicMeasures

import Data.Char
import System.Directory
import System.IO.Unsafe
import Data.Word
import Data.List
import Data.Ord
import Text.Show

-------------------------------------------------------------------------------------------
-------------------------------------------MENU--------------------------------------------
menu :: IO()
menu = do
		putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")	
		putStrLn "Bienvenido al menú."
		putStrLn "Selecciona la opcion que deseas escoger escribiendo el número correspondiente."
		menu' getArticles
--
menu' :: [Article] -> IO()
menu' articles = do				
					putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")	
					putStrLn "1. Mostrar los títulos de los artículos ordenados alfabéticamente y publicados en un año dado."
					putStrLn "2. Mostar el listado de revistas en las que se han publicado los artículos de toda la colección."
					putStrLn "3. Dado un acrónimo, buscarlo en los diferentes artículos y mostrar los títulos de aquellos que contengan el acrónimo."
					putStrLn "4. Dado el nombre de una revista y un acrónimo, mostrar los títulos de los artículos publicados en dicha revista que contengan el acrónimo."
					putStrLn "5. Dado un año de publicación, mostrar para cada artículo publicado en ese año el listado de acrónimos que contiene acompañados de sus formas expandidas."
					putStrLn "6. Dado un identificador del artículo, mostrar un listado de los acrónimos que contiene, acompañado del número de veces que aparece cada acrónimo en el artículo."
					putStrLn "7. Mostrar los títulos e identificador de todos aquellos artículos que no contengan ningún acrónimo."
					putStrLn "8. Dado el nombre de una revista, mostrar toda la información de los artículos publicados en dicha revista."
					putStrLn "9. Tratar de agrupar los artículos que se refieran a la misma enfermedad o a la misma temática."
					putStrLn "10. Mostrar los artículos que son iguales."
					putStrLn "11. Dado un acrónimo mostrar repeticiones."
					putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")	
					option <- getLine
					case option of
						"1" -> do
							putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
							putStrLn "Introduce el año: "
							year <- getLine
							exercise1 articles (read year) 
						"2" -> do
							exercise2 articles 
						"3" -> do
							putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
							putStrLn "Introduce el acronimo: "
							acronym <- getLine
							exercise3 articles (allUpper (acronym))
						"4" -> do
							putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
							putStrLn "Introduce el nombre de la revista: "
							magazine <- getLine
							putStrLn "Introduce el acrónimo: "
							acronym <- getLine
							exercise4 articles magazine (allUpper (acronym))
						"5" -> do
									putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
									putStrLn "Introduce el año: "
									year <- getLine
									exercise5 articles (read year) 
						"6" -> do
									putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
									putStrLn "Introduce la id del artículo: "
									identificator <- getLine
									exercise6 articles identificator 
						"7" -> do
									exercise7 articles
						"8" -> do
									putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
									putStrLn "Introduce el nombre de la revista: "
									magazine <- getLine
									exercise8 articles magazine
						"9" -> do
									putStrLn "Procesando, por favor espere..."
									exercise9 articles
						"10" -> do
									exercise10 articles
									
						"11" -> do
									putStrLn "Introduce el acrónimo: "
									acronym <- getLine
									exercise11 articles acronym 
						otherwise -> (menu' articles)
					menu' articles

--------------------------------------EJERCICIOS-------------------------------------------
-------------------------------------------------------------------------------------------
--1. Mostrar los títulos de los artículos ordenados alfabéticamente y publicados en un año
--dado
exercise1 :: [Article] -> Year -> IO()
exercise1 articles year = do
							putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
							printList (sort (getArticleTitlesByYear articles year))

getArticleTitlesByYear :: [Article] -> Year -> [ArticleTitle]
getArticleTitlesByYear [] year= []
getArticleTitlesByYear (x:xs) year = if (getYear x) == year then
								(getArticleTitle x):(getArticleTitlesByYear xs year)
							else
								getArticleTitlesByYear xs year					
																		
-------------------------------------------------------------------------------------------
--2. Mostar el listado de revistas en las que se han publicado los artículos de toda la
--colección
exercise2 :: [Article] -> IO()
exercise2 articles = do
						putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
						printList (getMagazines articles)

getMagazines :: [Article] -> [Magazine]
getMagazines [] = []
getMagazines (x:xs)= nub(((getMagazine x) : (getMagazines xs)))

-------------------------------------------------------------------------------------------
--3. Dado un acrónimo, buscarlo en los diferentes artículos y mostrar los títulos de
--aquellos que contengan el acrónimo

exercise3 :: [Article] -> Acronym -> IO()
exercise3 articles acronym = if (isAcronym acronym) then
								printList (nub (containsAcronym articles acronym []))
							 else
							 	print ""

containsAcronym :: [Article]-> Acronym -> [ArticleTitle] -> [ArticleTitle]
containsAcronym [] acronym articles = articles
containsAcronym (x:xs) acronym articles = if (articleContainsAcronym acronym (getSections x)) then 
									 containsAcronym xs acronym ((getArticleTitle x):articles)
									 else
									 	containsAcronym xs acronym articles

-------------------------------------------------------------------------------------------
--4. Dado el nombre de una revista y un acrónimo, mostrar los títulos de los artículos
--publicados en dicha revista que contengan el acrónimo.

exercise4 :: [Article] -> Magazine -> Acronym -> IO()
exercise4 articles magazine acronym = do
										putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
										printList (containsAcronym' articles magazine acronym [])

containsAcronym' :: [Article] -> Magazine -> Acronym -> [ArticleTitle] -> [ArticleTitle]
containsAcronym' [] magazine acronym articles = articles
containsAcronym' (x:xs) magazine acronym articles= if ((getMagazine x) == magazine) && (articleContainsAcronym acronym (getSections x)) then
														containsAcronym' xs magazine acronym ((getArticleTitle x):articles)
													 else
													 	containsAcronym' xs magazine acronym articles



-------------------------------------------------------------------------------------------
--5. Dado un año de publicación, mostrar para cada artículo publicado en ese año el listado
--de acrónimos que contiene acompañados de sus formas expandidas

exercise5 :: [Article]->Year->IO()
exercise5 [] year = print ""
exercise5 articles year = do
	putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
	printExpandedFormList (exercise5' articles (articlesInYear articles year))
-------------------------------------------------------------------------------------------
--------------------------Tipos de datos auxiliares----------------------------------------
--TitleAndAcronyms: muestra por cada artículo su título y una lista de acrónimos
data TitleAndAcronyms = TAA ArticleTitle [AcronymAndExpandedForm]

instance Eq TitleAndAcronyms where
	(TAA title acronyms) == (TAA title2 acronyms2) = (title==title2) 

instance Show TitleAndAcronyms where
	 show (TAA title acronyms) = title ++ ": Acronyms: " ++ show acronyms++"\n"

--AcronymAndExpandedForm: acrónimo y su forma expandida
data AcronymAndExpandedForm = AAEF Acronym String

instance Show AcronymAndExpandedForm where
	 show (AAEF acronym expanded) =  acronym ++ "= " ++ (capitalize (expanded))
	 
-------------------------------------------------------------------------------------------
printExpandedFormList :: [TitleAndAcronyms] -> IO()
printExpandedFormList [] = putStr ""
printExpandedFormList ((TAA title acronyms):xs)= do
	putStrLn title
	printAcronyms acronyms
	printExpandedFormList xs							

printAcronyms :: [AcronymAndExpandedForm] -> IO()
printAcronyms [] = putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
printAcronyms (x:xs) = do
	print x
	printAcronyms xs


--Por cada artículo crea un TitleAndAcronyms
exercise5' :: [Article] -> [Article] -> [TitleAndAcronyms]
exercise5' articles [] = []
exercise5' articles (x:xs) = [(TAA (getArticleTitle x) (getArticleAcronymsAndExpanded articles (articleAcronyms (getSections x))))]++(exercise5' articles xs) 

--Dada una lista de acrónimos devuelve una lista de acrónimos junto con su forma expandida
getArticleAcronymsAndExpanded :: [Article] -> [Acronym] -> [AcronymAndExpandedForm]
getArticleAcronymsAndExpanded articles []=[]
getArticleAcronymsAndExpanded articles (x:xs) = [(AAEF x (getExpandedFromList x (getAllAcronymsAndExpanded articles (nub(getAllAcronyms articles)))))]++(getArticleAcronymsAndExpanded articles xs)

--Dado un acrónimo busca en la lista de acrónimos con forma expandida su forma expandida
getExpandedFromList :: Acronym -> [AcronymAndExpandedForm] -> String
getExpandedFromList acronym1 ((AAEF acronym2 extended):xs) = if acronym1==acronym2 then
																extended
															else 
																getExpandedFromList acronym1 xs

--Devuelve todos las formas expandidas
getAllAcronymsAndExpanded :: [Article] -> [Acronym] -> [AcronymAndExpandedForm]
getAllAcronymsAndExpanded articles [] = []
getAllAcronymsAndExpanded articles (x:xs) = [(AAEF x (findExpandedFormArticles articles articles x))]++(getAllAcronymsAndExpanded articles xs) 

--Dado un acrónimo busca su forma expandida, primero en el artículo donde se encuentra 
--y luego, si no se encuentra su forma expandida ahí, busca en los demás
findExpandedFormArticles :: [Article] -> [Article] -> Acronym -> String
findExpandedFormArticles articles [] acronym = predefinedAcronym acronym												
findExpandedFormArticles articles (x:xs) acronym = if (articleHasAcronym (articleAcronyms (getSections x)) acronym) then
														if (expandedForm' articles (getSections x) acronym)=="" then
																findExpandedFormArticles articles xs acronym
														else
																expandedForm' articles (getSections x) acronym								
										  		   else
										  		   		
														findExpandedFormArticles articles xs acronym

--Busca el articulo correspondiente y actua sobre sus secciones	encontrando su forma expandida						
expandedForm :: [Article] -> ArticleTitle-> Acronym -> String
expandedForm articles title acronym = finalExpandedForm articles (expandedForm' articles (getSections (getArticleByTitle articles title)) acronym) acronym

--Comprueba si se encuentra laforma extendida en el articulo, sino busca en los demás
finalExpandedForm :: [Article] -> String -> String -> String
finalExpandedForm articles expandedForm acronym = if (expandedForm=="") then
														if (findExpandedFormArticles articles articles acronym)/="" then
												  			findExpandedFormArticles articles articles acronym
												  		else
												  			expandedForm
												  else
														expandedForm
														
--Encuentra la seccion en la que se encuentra el acronimo y la separa en palabras
expandedForm' :: [Article] -> Sections -> Acronym -> String
expandedForm' articles [] acronym = []
expandedForm' articles (x:xs) acronym = if (isInfixOf ("("++acronym++")") x) then
											if (length (lines x))>1 then										
												findExpandedForm articles (words ((lines x)!!1)) acronym 0 (words ((lines x)!!1))
											else
												findExpandedForm articles (words x) acronym 0 (words x)
										else
											expandedForm' articles xs acronym

--Dado un acrónimo  te busca en la lista de formas expandidas su significado
findExpandedAcronym :: Acronym -> [AcronymAndExpandedForm] -> String
findExpandedAcronym acronym expandedList = if (predefinedAcronym acronym)=="" then
												capitalize (findExpandedAcronym' acronym expandedList)
											else
												predefinedAcronym acronym
												
--Busca la forma expandida de un acrónimo en la lista de formas expandidas
findExpandedAcronym' :: Acronym -> [AcronymAndExpandedForm] -> String
findExpandedAcronym' acronym1 [] = []
findExpandedAcronym' acronym1 ((AAEF acronym2 extended):xs) = if (acronym1==acronym2) then
																	extended
															 else 
															 	findExpandedAcronym' acronym1 xs 

--Dada una lista de articulos devuelve sus expandidos
getAcronymsAndExpanded :: [Article] -> [Article] -> [AcronymAndExpandedForm]
getAcronymsAndExpanded articles [] = []
getAcronymsAndExpanded articles (x:xs) =(newAcronymsAndExpanded articles (articleAcronyms (getSections x)))++(getAcronymsAndExpanded articles xs)

--Recibe la lista de acronimos de un articulo y devuelve formas extendidas
newAcronymsAndExpanded :: [Article] -> [Acronym] -> [AcronymAndExpandedForm]
newAcronymsAndExpanded articles [] = []
newAcronymsAndExpanded articles (x:xs) = [(AAEF x (findExpandedFormArticles articles articles x))]++(newAcronymsAndExpanded articles xs)
					  										
--Dada la seccion separada en palabras, encuentra el acronimo entre paréntesis, devolviendo
--tantas palabras antes del acronimo como la longitud del mismo
findExpandedForm :: [Article] -> [String] -> Acronym -> Int -> [String] -> String
findExpandedForm articles [] acronym position section = []
findExpandedForm articles (x:xs) acronym position section= if (isInfixOf ("("++acronym++")") x) then--Coge las ultimas palabras antes del acrónimo
																	if ((head (reverse acronym))=='-') then--Si acaba por - se quita para la búsqueda
																		if (hasParenthesis (take ((length acronym)-1) (reverse (take position section)) ))||(hasInvalidWord (take ((length acronym)-1) (reverse (take position section)) )) then
															   				intercalate " " (findExpandedForm' articles (removeQuotes (reverse (take (length acronym) (reverse (take position section)) ))) (reverse (tail (reverse acronym))) 0)
															   			else
															   				intercalate " " (findExpandedForm' articles (removeQuotes (reverse (take ((length acronym)-1) (reverse (take position section)) ))) (reverse (tail (reverse acronym))) 0)
																	else--Si no acaba en -
																		if (hasParenthesis (take (length acronym) (reverse (take position section)) ))||(hasInvalidWord (take (length acronym) (reverse (take position section)) )) then
															   				intercalate " " (findExpandedForm' articles (removeQuotes (reverse (take ((length acronym)+1) (reverse (take position section)) ))) acronym 0)
															   			else if (length (deleteEmptys (split acronym)))>0 then
															   				intercalate " " (findExpandedForm' articles (removeQuotes (reverse (take ((length acronym)-(reduceSearch  ((reverse (take ((length acronym)+1) (reverse (take position section)) ))) acronym )) (reverse (take position section)) ))) acronym 0)															   									   			
															   			else
															   				if (invalidWords (reverse (take (length acronym) (reverse (take position section)) )))/=0 then
															   					intercalate " " (findExpandedForm' articles (removeQuotes (reverse (take ((length acronym)+1+(invalidWords (reverse (take (length acronym) (reverse (take position section)) )))) (reverse (take position section)) ))) acronym 0)
															   				else	
															   					intercalate " " (findExpandedForm' articles (removeQuotes (reverse (take (length acronym) (reverse (take position section)) ))) acronym 0)
															else--Si no es el acrónimo entre paréntesis sigue buscando en la sección
																   	findExpandedForm articles xs acronym (position+1) section

--Devuelve la forma expandida dado el conjunto de palabras que va antes del acrónimo entre parentesis
findExpandedForm' :: [Article] -> [String] -> Acronym -> Int -> [String]
findExpandedForm' articles [] acronym position = []
findExpandedForm' articles (x:xs) acronym position = if ((toUpper (head x))==(acronym!!position)) then 
															if (similars (firstLetter ( (deleteEmptys (split x))++xs)) acronym 50)||(position/=0) then													
																		if position==(length acronym)-1 then
																			--Si se está analizando la última letra del acrónimo se añade la palabra a la forma extendida
																			[x]++(findExpandedForm' articles xs acronym (position))
																		else
																			[x]++(findExpandedForm' articles xs acronym (position+ (length (split x))))
																		else if (xs==[])&&(position==0) then
																			[x]
																		else if (position==0)&&((findExpandedAcronym (getSecondTuple (splitAt (position+1) acronym)) (getAllAcronymsAndExpanded articles (getAllAcronyms articles)))/="") then
																		--Si quitando la primera letra existe el acrónimo lo suma a su expansión
																			[x]++(words (findExpandedAcronym (getSecondTuple (splitAt (position+1) acronym)) (getAllAcronymsAndExpanded articles (getAllAcronyms articles))))
																		else if (xs==[]) &&(isRomanNumeral x)&&(x==(getSecondTuple (splitAt position acronym))) then
																		--Si la ultima palabra es un número y sólo quedan números en el acrónimo, y es igual lo añade
																			[x]
																		else--Si no tiene nada que ver con el acronimo pasa de palabra
																			findExpandedForm' articles xs acronym position
													    	else--Si no es igual la primera letra
													    		if (containsParenthesis x)||(position==0) then--Si contiene un paréntesis no coge la palabra										    	
																	findExpandedForm' articles xs acronym position
																else if (x=="of")||(x=="for")||(x=="and") then--Si tiene of o for la añade
																	[x]++findExpandedForm' articles xs acronym position
														else--Si no es nada igual																
																	[x]++(findExpandedForm' articles xs acronym (position+1))

--Reduce la búsqueda si el acrónimo es de tipo "x-y" y la forma expandida contiene x o y
reduceSearch :: [String] -> Acronym -> Int
reduceSearch [] acronym = 0
reduceSearch (x:xs) acronym = (reduceSearch' x (split acronym))+(reduceSearch xs acronym)

reduceSearch' :: String -> [Acronym] -> Int
reduceSearch' acronym [] = 0
reduceSearch' acronym (x:xs) = if (isInfixOf x acronym) then
									(length x)+(reduceSearch' acronym xs)
								else
									reduceSearch' acronym xs

--Separa una palabra que contenga "-"
split :: String -> [String]
split [] = []
split x = 	if (length (split' x []))>1 then
				if (isAcronym ( (deleteEmptys (split' x []))!!0))||(isAllUpper (deleteEmptys (split' x []))) then
					(split' x [])
				else
					[x]
			else if (isUpper (head x)) then
				[x]			
			else
				(split' x [])

split' [] t = [t]
split' (a:l) t = if a=='-' then (t:split' l []) else split' l (t++[a])

--Dado un conjunto de palabras devuelve la primera letra las que sean válidas
firstLetter:: [String] -> String
firstLetter [] = []
firstLetter (x:xs) = if (isRomanNumeral x) then
							x++((firstLetter (tail (deleteEmptys (split x))))++(firstLetter xs))
					 else if (head x)=='“' then
					 		(toUpper (head (tail (head (deleteEmptys (split x)))))):((firstLetter (tail (deleteEmptys (split x))))++(firstLetter xs))
					 else if (length (deleteEmptys (split x)))>1 then
					 		(firstLetter' (deleteEmptys (split x)))++(firstLetter xs)					 								 		
					 else 
							(toUpper (head (head(deleteEmptys (split x))))):((firstLetter (tail (deleteEmptys (split x))))++(firstLetter xs))

firstLetter' :: [String] -> String
firstLetter' [] = []
firstLetter' (x:xs) = if (isAcronym x) then
						x++(firstLetter' xs)
					  else
					  	[toUpper (head x)]++(firstLetter' xs)	
					  	
-------------------------------------------------------------------------------------------
--6. Dado un identificador del artículo, mostrar un listado de los acrónimos que contiene, 
--acompañado del número de veces que aparece cada acrónimo en el artículo
-------------------------------------------------------------------------------------------
--------------------------Tipos de datos auxiliares----------------------------------------
--AcronymAndNumber contiene un acrónimo y el número de veces que aparece en un artíclo
data AcronymAndNumber = AAN Acronym Integer 

instance Eq AcronymAndNumber where
	(AAN acronym times) == (AAN acronym2 times2) = (acronym==acronym2) 

instance Show AcronymAndNumber where
	 show (AAN acronym times) = acronym ++ ": " ++ show times ++ " times"
-------------------------------------------------------------------------------------------	
				 
exercise6 :: [Article] -> Id -> IO()
exercise6 articles identificator = do
	putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
	printAcronymsAndNumber (acronymListId articles identificator)						

printAcronymsAndNumber :: [AcronymAndNumber] -> IO()
printAcronymsAndNumber [] = putStr ""
printAcronymsAndNumber (x:xs) = do
	print x
	printAcronymsAndNumber xs

--Dado un identificador de un artículo devuelve los acrónimos que tienen con sus repeticiones
acronymListId :: [Article] -> Id -> [AcronymAndNumber]
acronymListId [] identificator = []
acronymListId (x:xs) identificator = if (getId x)==identificator then
											nub ((acronymListIdAux (articleAcronymsFinal (articleAcronymsRepeated (getSections x)) []) (articleAcronymsFinal (articleAcronyms (getSections x)) [])))
									   else
											acronymListId xs identificator

--Recoge una lista de acrónimos los convierte a tipo AcronymAndNumber
acronymListIdAux :: [Acronym] -> [Acronym] -> [AcronymAndNumber]
acronymListIdAux acronyms [] = []
acronymListIdAux acronyms (x:xs) = (AAN x (timesRepeated acronyms x)):(acronymListIdAux acronyms xs)

--Elimina los símbolos inválidos 						
articleAcronymsFinal :: [Acronym] -> [Acronym] -> [Acronym]
articleAcronymsFinal [] acronyms= []
articleAcronymsFinal (x:xs) acronyms = (deleteSign x ""):(articleAcronymsFinal xs acronyms)
																						
--Dado un acrónimo y la lista de acrónimos de un artículo devuelve las veces que se repite	
timesRepeated :: [Acronym] -> Acronym -> Integer
timesRepeated [] _ = 0
timesRepeated (x:xs) number = if (x==number) then
								1+(timesRepeated xs number)
							  else
							    timesRepeated xs number

-------------------------------------------------------------------------------------------
--7. Mostrar  los  títulos  e  identificador  de  todos  aquellos  artículos  que no
--contengan ningún acrónimo. 
-------------------------------------------------------------------------------------------
--------------------------Tipos de datos auxiliares----------------------------------------
--TitleAndId: título de un artículo y su identificador
data TitleAndId = TAI ArticleTitle Id
instance Show TitleAndId where
	 show (TAI title identification) = "Title: "++ title ++ "\nId: " ++ identification
-------------------------------------------------------------------------------------------

exercise7 :: [Article] -> IO()
exercise7 articles = printTitleAndId (noAcronyms articles)

--Devuelve el título y la id de los artículos que no tienen acrónimos
noAcronyms :: [Article] -> [TitleAndId]
noAcronyms [] = []
noAcronyms (x:xs)= if (length (articleAcronyms (getSections x)))==0 then
						(TAI (getArticleTitle x) (getId x)):(noAcronyms xs)
					else
						noAcronyms xs

printTitleAndId :: [TitleAndId] -> IO()
printTitleAndId [] = putStr ""
printTitleAndId (x:xs) = do
						putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
						print x
						putStr "\n"
						printTitleAndId xs
-------------------------------------------------------------------------------------------
--8. Dado el nombre de una revista, mostrar toda la información de los artículos publicados 
--en dicha revista.
-------------------------------------------------------------------------------------------

exercise8 :: [Article] -> Magazine -> IO()
exercise8 articles magazine = infoArticle (sortArticlesByTitle (getArticlesByMagazine articles magazine)) 

infoArticle :: [Article] -> IO()
infoArticle [] = putStr ""
infoArticle (x:xs)=  do
	putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
	print x						
	infoArticle xs
	
sortArticlesByTitle :: [Article] -> [Article]
sortArticlesByTitle []     = []
sortArticlesByTitle ((A m i y t s):xs) = sortArticlesByTitle lesser ++ [A m i y t s] ++ sortArticlesByTitle greater
    where
        lesser  = [ (A m2 i2 y2 t2 s2) | (A m2 i2 y2 t2 s2) <- xs, t2 < t ]
        greater = [ (A m2 i2 y2 t2 s2) | (A m2 i2 y2 t2 s2) <- xs, t2 >= t ]
-------------------------------------------------------------------------------------------
--9. Tratar de agrupar los artículos que se refieran a la misma enfermedad o a la misma 
--temática.
-------------------------------------------------------------------------------------------
--------------------------Tipos de datos auxiliares----------------------------------------
--IdAndArticleTitle: id de un artículo y su título
data IdAndArticleTitle = IAAT Id ArticleTitle
instance Eq IdAndArticleTitle where
	(IAAT identificator articleTitle) == (IAAT identificator2 articleTitle2) = (identificator)==(identificator2) 
instance Show IdAndArticleTitle where
	 show (IAAT identificator articleTitle) =  identificator ++" - " ++  articleTitle

--Cluster: categoría, que contiene el título de la categoría y los artículos que contiene
data Cluster = C ClusterTitle [IdAndArticleTitle]
type ClusterTitle = String 

getIdentificators :: Cluster -> [IdAndArticleTitle]
getIdentificators (C title identificators) = identificators

instance Show Cluster where
	 show (C title identificators) = "Articles referring to "++ title ++ (printArticlesCluster identificators)

instance Eq Cluster where
	(C title identificators) == (C title2 identificators2) = (length identificators)==(length identificators2) 

instance Ord Cluster where
			compare x y= compare (length (getIdentificators x)) (length (getIdentificators y))

--AcronymsAndTimesRepeated lo uso para ver las veces que se repiten los acronimos en la lista de articulos
data AcronymsAndTimesRepeated = AATR Acronym Int deriving Show
instance Eq AcronymsAndTimesRepeated where
	(AATR acronym1 times) == (AATR acronym2 times2) = (times==times2)
	
instance Ord AcronymsAndTimesRepeated where
			AATR acronym1 times1< AATR acronym2 times2 = times1<times2 
			AATR acronym1 times1 > AATR acronym2 times2 = times1>times2	
-------------------------------------------------------------------------------------------			

exercise9 :: [Article] -> IO()
exercise9 articles = printCategories ((getCategories articles)++[getOthersCategory articles]) (getAllAcronymsAndExpanded articles (getAllAcronyms articles))

printCategories :: [Cluster] -> [AcronymAndExpandedForm] -> IO()
printCategories [] expandedForms = putStr ""
printCategories ((C title identificators):xs) expandedForms = do
						putStrLn ("-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-"++"-")
						if (findExpandedAcronym title expandedForms=="") then
							putStrLn title
						else
							putStrLn (findExpandedAcronym title expandedForms) 
						putStr (printArticlesCluster identificators)
						printCategories xs expandedForms

printArticlesCluster :: [IdAndArticleTitle] -> String
printArticlesCluster [] = []
printArticlesCluster (x:xs) = do
	(show x)++"\n"++(printArticlesCluster xs )

getCategories :: [Article] -> [Cluster]
getCategories [] = []
getCategories articles = reverse (sort (deleteEmptyCategories (getCategories' articles (clusters articles) [])))

getCategories' :: [Article] -> [Cluster] -> [IdAndArticleTitle] -> [Cluster]
getCategories' articles [] ids = []
getCategories' articles ((C title identificators):xs) ids = [(C title (filterIds identificators ids))]++(getCategories' articles xs ((filterIds identificators ids)++ids))

--Elimina las categorias que no tienen ningún artículo dentro
deleteEmptyCategories :: [Cluster] -> [Cluster]
deleteEmptyCategories [] = []
deleteEmptyCategories ((C title identificators):xs) = if (identificators==[]) then
					  	deleteEmptyCategories xs
					  else
					  	[(C title identificators)]++(deleteEmptyCategories xs)

--Dadas las ids de un cluster, elimina las que ya estan categorizadas
filterIds :: [IdAndArticleTitle] -> [IdAndArticleTitle] ->[IdAndArticleTitle]
filterIds [] _ = []
filterIds ((IAAT identificator articleTitle):xs) ids = if (elem identificator (getIdAndArticleTitleIds ids)) then
							filterIds xs ids
					   else
					   		[(IAAT identificator articleTitle)]++(filterIds xs ids) 

--Devuelve las ids
getIdAndArticleTitleIds :: [IdAndArticleTitle] -> [Id]
getIdAndArticleTitleIds [] = []
getIdAndArticleTitleIds ((IAAT identificator articleTitle):xs) = [identificator]++(getIdAndArticleTitleIds xs)

--Crea una categoría en el que están los elementos descategorizados
getOthersCategory :: [Article] -> Cluster
getOthersCategory articles = C "Other category" (idToIdAndArticleTitle articles (unCategorizedArticles (getIds articles) (getOthersCategory' (getCategories articles))))

--Devuelve todas las ids de los artículos categorizados
getOthersCategory' :: [Cluster] -> [Id]
getOthersCategory' [] = []
getOthersCategory' ((C title identificators):xs) = (getIdAndArticleTitleIds identificators)++(getOthersCategory' xs)

idToIdAndArticleTitle :: [Article] -> [Id] -> [IdAndArticleTitle]
idToIdAndArticleTitle articles [] = []
idToIdAndArticleTitle articles (x:xs) = [(IAAT x (getArticleTitle (getArticleById articles x)))]++(idToIdAndArticleTitle articles xs)

--Devuelve las ids de los artículos descategorizados
unCategorizedArticles :: [Id] -> [Id] -> [Id]--
unCategorizedArticles clusterId [] = clusterId
unCategorizedArticles clusterId (x:xs) = unCategorizedArticles (unCategorizedArticles' clusterId x []) xs

--Dado un array de ids, te quita las ids que ya se han categorizado
unCategorizedArticles' :: [Id] -> Id -> [Id] -> [Id]
unCategorizedArticles' [] identificator final = final
unCategorizedArticles' (x:xs) identificator final= if (x==identificator) then
														unCategorizedArticles' xs identificator final
											  		else
														unCategorizedArticles' xs identificator (x:final)

--Devuelve un array de todos los clusters
clusters :: [Article] -> [Cluster]
clusters articles = reverse (sort (nub (clustersAux (clusters' articles (acronymsRepetitions articles)) (clusters' articles (acronymsRepetitions articles))))) 	

--Concatenan los clusters																				 	  
clustersAux :: [Cluster] -> [Cluster] -> [Cluster]
clustersAux [] _= []
clustersAux (x:xs) clusterList = [concatenateCluster x clusterList]++(clustersAux xs clusterList)

--Concatena un cluster con otro
concatenateCluster :: Cluster -> [Cluster] -> Cluster
concatenateCluster (C title identificators) [] = (C title (nub (identificators)))
concatenateCluster (C title identificators) ((C title2 identificators2):xs) = if (title==title2) then
																			concatenateCluster (C title (identificators++identificators2)) xs
																	  else
																	  		concatenateCluster (C title identificators) xs
																				
--Recibe un listado de artículos y un listado de acrónimos y sus veces repetidos 
--y devuelve una lista de clusters con cada acrónimo asociado a la id del artículo
--al que pertenece
clusters' :: [Article] -> [AcronymsAndTimesRepeated] -> [Cluster]
clusters' [] aatr = []
clusters' (x:xs) aatr = (clusters'' x aatr)++(clusters' xs aatr)

--Por cada artículo crea varios cluster con la id del artículo y cada acrónimo
clusters'' :: Article -> [AcronymsAndTimesRepeated] -> [Cluster]
clusters'' _ [] = []
clusters'' article ((AATR acronym times):xs) = if (articleHasAcronym (articleAcronyms (getSections article)) acronym) then
													(C acronym [(IAAT (getId article) (getArticleTitle article))]):(clusters'' article xs)
												else
													clusters'' article xs
													
--Dados los artículos devuelve la lista de acrónimos y sus repeticiones
acronymsRepetitions :: [Article] -> [AcronymsAndTimesRepeated]
acronymsRepetitions [] = []
acronymsRepetitions articles =  (createRepetitions (getAllAcronyms articles) (getAllAcronyms articles))

--Dados todos los acrónimos devuelve una clase con los acrónimos y sus repeticiones
createRepetitions :: [Acronym] -> [Acronym] -> [AcronymsAndTimesRepeated]
createRepetitions _ [] = []
createRepetitions acronyms (x:xs) = nub ((AATR x (repetitionsAcronym acronyms x)):(createRepetitions acronyms xs))

--Devuelve las repeticiones de un acrónimo concreto
repetitionsAcronym :: [Acronym] -> Acronym -> Int
repetitionsAcronym [] _ = 0
repetitionsAcronym (x:xs) acronym = if (x==acronym) then
											1+(repetitionsAcronym xs acronym)
									else 
										repetitionsAcronym xs acronym	
										
-------------------------------------------------------------------------------------------
--10. Mostrar los artículos que son iguales						
data IdMagazineArticleTitle = IMA Id Magazine ArticleTitle					
	
instance Show IdMagazineArticleTitle where
	 show (IMA identificator magazine articleTitle) = identificator++": " ++magazine ++ articleTitle
	 
	 
instance Eq IdMagazineArticleTitle where
	(IMA identificator magazine articleTitle) == (IMA identificator2 magazine2 articleTitle2) = (identificator==identificator2)	 
		

exercise10 :: [Article] -> IO()
exercise10 articles = printExercise10 (exercise10' articles articles)

printExercise10 :: [[IdMagazineArticleTitle]] -> IO()
printExercise10 [] = putStrLn ""
printExercise10 (x:xs) = do
						print (head x)
						putStrLn "///////////////////////////"
						printExercise10 xs
			
printExercise10' :: [IdMagazineArticleTitle] -> IO()
printExercise10' [] = putStr ""
printExercise10' (x:xs) = do
						print x	
						printExercise10' xs							
										
--Dados los articulos, compara cada uno a los demás llamando al auxiliar exercise10Aux										
exercise10' :: [Article] -> [Article] -> [[IdMagazineArticleTitle]]
exercise10' articles [] = []
exercise10' articles (x:xs) =  deleteEmptyArray (nub ([exercise10Aux articles x []]++exercise10' articles xs)) []

--Por cada articulo busca en los demas si alguno tiene el mismo titulo que el, y lo guarda
exercise10Aux :: [Article] -> Article -> [IdMagazineArticleTitle] -> [IdMagazineArticleTitle]
exercise10Aux [] (A m2 i2 y2 a2 s2) [] = []
exercise10Aux [] (A m2 i2 y2 a2 s2) iguales = if (length iguales ==1) then
												[]
											  else
												iguales ++ [(IMA i2 m2 a2)]
exercise10Aux ((A m1 i1 y1 a1 s1):xs) (A m2 i2 y2 a2 s2) iguales = if (m1==m2)&&(a1==a2) then
															 exercise10Aux xs (A m2 i2 y2 a2 s2) ([(IMA i1 m1 a1)]++iguales)
														 else
														 	 exercise10Aux xs (A m2 i2 y2 a2 s2) iguales
														 	 
deleteEmptyArray :: [[IdMagazineArticleTitle]] -> [[IdMagazineArticleTitle]] -> [[IdMagazineArticleTitle]]
deleteEmptyArray [] filtered = filtered
deleteEmptyArray (x:xs) filtered = if (x== []) then
									deleteEmptyArray xs filtered
								  else 
								  	deleteEmptyArray xs ([x]++filtered)	
								  	
								  	
-------------------------------------------------------------------------------------------
--11. Mostrar acrónimo acompañado del número de articulos que lo contienen y sus identificadores			
data AcronymArticlesIdentificators = AAI Acronym Integer [Id]				
	
exercise11 :: [Article] -> Acronym -> IO()
exercise11 articles acronym = do
						putStrLn acronym
						print (length (idsAcronym acronym articles []))
						putStrLn "Ids: "
						print (idsAcronym acronym articles [])

idsAcronym :: Acronym -> [Article] -> [Id] -> [Id]
idsAcronym acronym [] ids = ids
idsAcronym acronym (x:xs) ids = if (articleContainsAcronym acronym (getSections x)) then
								idsAcronym acronym xs ([(getId x)]++ids)
							else
								idsAcronym acronym xs ids																	  						  	