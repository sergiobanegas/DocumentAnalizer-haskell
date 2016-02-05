module GetArticles where
import Article
import System.Directory
import System.IO.Unsafe

getArticles :: [Article]
getArticles = arrayArticles (arrayLines (unsafePerformIO readDirectory ) [])

--Función principal para conseguir el array de contenidos
readDirectory :: IO [String] 
readDirectory = do
	setCurrentDirectory "papers"
	path <- getCurrentDirectory
	files <- getDirectoryContents path
	let filteredFiles = filter (flip notElem [".",".."]) files
	return (readArticles filteredFiles)
	
--Lee el artículo y lo devuelve en un string
readArticle :: String  -> IO String
readArticle file = do
	articleContent <-(readFile file)
	return articleContent

--Dado el listado de archivos te devuelve un array con los contenidos de cada archivo
readArticles :: [String] -> [String]
readArticles list = readArticlesAux [] list

--Función auxiliar para leer artículos
readArticlesAux:: [String] -> [String] -> [String]
readArticlesAux _ [] = []
readArticlesAux list (x:xs) = (unsafePerformIO(readArticle x)):(readArticlesAux list xs)
	
--Dado un array de arrays de líneas, te devuelve un array de Articulos, llamando a createArticle en cada posicion
arrayArticles :: [[String]] -> [Article]
arrayArticles [] = []
arrayArticles (l:ls) = (createArticle l (length l) 0 0 "Magazine" "ID" 0 "articleTitle" [] ""):(arrayArticles ls)

--Dado un array de las líneas de un artículo, te lo convierte en un tipo Articulo
createArticle :: [String] -> Int -> Int -> Int -> String -> String -> Integer -> String -> [String] -> String -> Article
createArticle (l:ls) size line1 line2 magazine articleId year articleTitle sections aux= case ls of
							[] -> A magazine articleId year articleTitle sections
							otherwise -> case line1 of
										0 -> createArticle ls size (line1 + 1) line2 l articleId year articleTitle sections aux
										1 -> createArticle ls size (line1 + 1) line2 magazine l year articleTitle sections aux
										2 -> createArticle ls size (line1 + 1) line2 magazine articleId (read l :: Integer) articleTitle sections aux
										3 -> createArticle ls size (line1 + 1) line2 magazine articleId year articleTitle sections aux
										4 -> createArticle ls size (line1 + 1) line2 magazine articleId year l sections aux
										5 -> createArticle ls size (line1 + 1) line2 magazine articleId year articleTitle sections aux
										otherwise -> case line2 of
														0 -> createArticle ls size line1 (line2 + 1) magazine articleId year articleTitle sections (aux++(l++"\n"))
														otherwise-> if length l==2 then
																		createArticle ls size line1 0 magazine articleId year articleTitle (sections++[aux]) ""
																	else
																		createArticle ls size line1 line2 magazine articleId year articleTitle sections (aux++l)
																																																						
--Dado un array de strings (cada string es un artículo), lo convierte a array de array de líneas
arrayLines :: [String] -> [[String]] -> [[String]]
arrayLines [] result = result
arrayLines (l:ls) result= arrayLines ls ((lines l):result)  