module Acronym where
import Data.Char
import Data.List

type Acronym = String

isAcronym :: String -> Bool
isAcronym acronym = if (isRomanNumeral acronym) then
						False
					else if (isDigit (head acronym)) then
						False
					else if ((length acronym)==2) && (isDigit (acronym!!1))then
						False
					else if ((head acronym)=='(')&&((last acronym)==')') then
						if (isRomanNumeral (reverse (tail (reverse (tail acronym))))) then
							False
						else if (isDigit (head acronym)) then
							False
						else if ((length acronym)==2) && (isDigit (acronym!!1))then
							False
						else
							isAcronym' (reverse (tail (reverse (tail acronym))))
					else if ((head acronym)=='(') then
						if (isRomanNumeral (tail acronym)) then
							False
						else
							isAcronym' (tail acronym)
					else if ((last acronym)==',')||((last acronym)=='.')||((last acronym)==')') then
						if (isRomanNumeral (reverse (tail (reverse acronym)))) then
							False
						else 
							isAcronym' (reverse (tail (reverse acronym)))
					else
						isAcronym' acronym

isAcronym' :: Acronym -> Bool
isAcronym' [] = False
isAcronym' (x:[]) = False
isAcronym' (x:xs) = isAcronymAux (x:xs)


isAcronymAux :: Acronym -> Bool
isAcronymAux [] = False
isAcronymAux (x:[]) = if ((isUpper x)||(isDigit x)||(x=='-')) then
						True
					  else
					  	False					  	
isAcronymAux (x:xs) = if ((isUpper x)||(isDigit x)||(x=='-')) then
						isAcronymAux xs
					  else
					  	False

--Comprueba si el supuesto acrónimo sólo tiene números
isANumber :: Acronym -> Bool
isANumber [] = False
isANumber (x:[]) = isDigit x
isANumber (x:xs) = ((isDigit x)||(x=='-'))&&(isANumber xs)


--Si el supuesto acrónimo sólo tiene números no será acrónimo, si no es así, comprueba si
--contiene sólo mayúsculas y/o números		
isRomanNumeral :: Acronym -> Bool
isRomanNumeral [] = False
isRomanNumeral (x:xs) = if (x=='F') then
							isRomanNumeral' xs
						else
							if (xs==[]) then
								isRomanNumeral' [x]
							else
								if ((isRomanNumeral' [(head (reverse (x:xs)))])==False)&&( (head (tail (reverse (x:xs)) ))=='-') then
									 True
								else
									isRomanNumeral' (x:xs)

			
isRomanNumeral' :: Acronym -> Bool
isRomanNumeral' [] = True
isRomanNumeral' (x:xs) = if ((x=='X') || (x=='V') || (x=='I') || (x=='-'))||(isDigit x) then
							True && (isRomanNumeral' xs)
				   		else
				   			False

deleteSign :: Acronym -> Acronym -> Acronym
deleteSign [] acronimo = acronimo
deleteSign (x:xs) acronimo= if (x=='(')||(x==')')||(x==',')||(x=='.') then
								deleteSign xs acronimo
							else
								deleteSign xs (acronimo++[x])	

--Dada una tupla te devuelve el segundo elemento
getSecondTuple:: (String,String) -> String	
getSecondTuple (_,x) = x

--Mira si es un acrónimo predefinido y devuelve su forma expandida si lo es
predefinedAcronym :: String -> Acronym
predefinedAcronym acronym = case acronym of
								"DNA" -> "Deoxyribonucleic acid"
								"3D" -> "Three Dimensions"
								"UK" -> "United Kingdom"
								"US" -> "United States"
								"USA" -> "United States of America"
								"NJ" -> "New Jersey"
								"MA" -> "Massachusetts"
								"PA" -> "Pennsylvania"
								"CA" -> "California"
								"WI" -> "Wisconsin"
								otherwise -> "" 	