module Article where
	
	import Data.List
	import Acronym
	 
	type Magazine = String
	type Id = String
	type Year = Integer
	type ArticleTitle = String
	type Sections = [String]
		
	data Article = A Magazine Id Year ArticleTitle Sections
	
	instance Eq Article where
			(A m i y a s) == (A m2 i2 y2 a2 s2) = (y==y2) 
	
	instance Ord Article where
			A m i y a s < A m2 i2 y2 a2 s2 = y<y2 
			A m i y a s > A m2 i2 y2 a2 s2 = y>y2
	
	instance Show Article where
		show (A m i y a s) = "Title: "++a++"\nAbstract: "++(abstractText (head s))++"\nSection number: "++show ((length s)-1)++"\nSections:\n"++(printSections (tail s))
	
	--Gets
	getMagazine :: Article -> Magazine
	getMagazine (A magazine _ _ _ _) = magazine
			
	getYear :: Article -> Year
	getYear (A _ _ year _ _) = year
	
	getYears :: [Article] -> [Year]
	getYears [] = []
	getYears ((A _ _ y _ _):xs) = nub ([y]++(getYears xs))
	
	getId :: Article -> Id
	getId (A _ identificator _ _ _) = identificator
	
	--Listado de ids de artículos
	getIds :: [Article] -> [Id]
	getIds [] = []
	getIds ((A _ identificator _ _ _):xs) = ([identificator]++(getIds xs))
	
	getArticleTitle :: Article -> ArticleTitle
	getArticleTitle (A _ _ _ articleTitle _) = articleTitle
	
	getSections :: Article -> Sections
	getSections (A _ _ _ _ sections) = sections
	
	--Dado un título de un artículo te devuelve el artículo
	getArticleByTitle :: [Article] -> ArticleTitle -> Article
	getArticleByTitle (x:xs) title = if (getArticleTitle x)==title then
										x
									else
										getArticleByTitle xs title
	
	getArticlesByMagazine :: [Article] -> Magazine -> [Article]
	getArticlesByMagazine [] magazine = []
	getArticlesByMagazine (x:xs) magazine = if (getMagazine x)==magazine then
												[x]++(getArticlesByMagazine xs magazine)
											else
												getArticlesByMagazine xs magazine
	--Dada una id te devuelve el artículo
	getArticleById :: [Article] -> Id -> Article
	getArticleById (x:xs) identificator = if (getId x)==identificator then
												x
										  else
												getArticleById xs identificator
	
	--Te devuelven los artículos publicados en un año dado
	articlesInYear :: [Article] -> Year -> [Article]
	articlesInYear [] year = []
	articlesInYear (x:xs) year = if (getYear x)==year then
								[x]++(articlesInYear xs year)
							  else
							  	(articlesInYear xs year)
		
	--Te devuelven todos los acrónimos que tienen los artículos
	getAllAcronyms :: [Article] -> [Acronym]
	getAllAcronyms [] = []
	getAllAcronyms (x:xs) = (articleAcronyms (getSections x))++(getAllAcronyms xs)											
	
	--Dado el listado de acrónimos de un artículo, te dice si contiene un acrónimo dado
	articleHasAcronym :: [Acronym] -> Acronym -> Bool
	articleHasAcronym [] acronym = False
	articleHasAcronym	(x:xs) acronym = if (x==acronym) then
									True
								 else
								 	articleHasAcronym xs acronym		
												
	--Dadas las secciones de un artículo te dice si contiene un acrónimo
	articleContainsAcronym :: Acronym -> Sections -> Bool
	articleContainsAcronym acronym [] = False
	articleContainsAcronym acronym (x:xs) = (isInfixOf acronym x)||(articleContainsAcronym acronym xs)
	
	--Dadas las secciones de un artículo, te devuelve los acrónimos que contiene sin repetir	
	articleAcronyms :: Sections -> [Acronym]
	articleAcronyms [] = []
	articleAcronyms (x:xs) = nub ((sectionAcronyms x)++(articleAcronyms xs))
	
	--Dadas las secciones de un artículo, te devuelve los acrónimos que contiene contando repetidos
	articleAcronymsRepeated :: Sections -> [Acronym]
	articleAcronymsRepeated [] = []
	articleAcronymsRepeated (x:xs) = (sectionAcronyms x)++(articleAcronyms xs)
	
	--Dada una sección te devuelve los acrónimos que contiene
	sectionAcronyms :: String -> [Acronym]
	sectionAcronyms section = if (length (lines section))>1 then--Si la seccion no es el resumen
									sectionAcronymsAux (words ((lines section)!!1))--No busca en el título de esta
							  else
							  		sectionAcronymsAux (words section) 
	
	--Dadas las palabras que contiene una sección, te devuelve los acrónimos que contiene
	sectionAcronymsAux :: [String] -> [Acronym]
	sectionAcronymsAux [] = []
	sectionAcronymsAux (x:xs) = if (isAcronym x) then
									(deleteSign x ""):(sectionAcronymsAux xs)
								else
									sectionAcronymsAux xs	
			
	abstractText :: String -> String
	abstractText [] = []
	abstractText resumen = if (length (words resumen))>33 then
								(firstLines resumen 0 [])
		   	     			else
			 					resumen
	
	--Dado el resumen muestra las 33 primeras palabras
	firstLines :: String -> Integer -> String -> String
	firstLines [] wordsNumber first = first
	firstLines (x:xs) wordsNumber first= if (wordsNumber < 33) then
										if (x==' ') then
											firstLines xs (wordsNumber+1) (first++[x])
										else
											firstLines xs wordsNumber (first++[x])
									else
										first++"..."
	
	printSections :: Sections -> String
	printSections [] = []
	printSections (x:xs) = do
					((head (lines x))++"\n")++(printSections xs)
					