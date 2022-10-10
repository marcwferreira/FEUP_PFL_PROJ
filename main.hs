import Data.List
import Data.Char

main = do  
    putStrLn "What is the desired funcion?" 
    putStrLn "A - normalize polynomial"
    putStrLn "B - add polynomials"
    putStrLn "C - multiply polynomials"
    putStrLn "D - derivative of polynomial"
    name <- getLine 
    putStrLn ("Hello " ++ name)

split:: Eq a => a -> [a] -> [[a]]
split _ [] = []
split delimiter rcvString = takeWhile (\a -> a /= delimiter) rcvString : split delimiter (dropWhile (\b -> b == delimiter) (dropWhile (\b -> b /= delimiter) rcvString))

--Parser the string into different components
parserPolynomial:: String -> [String]
parserPolynomial [] = []
parserPolynomial rcvString = ([head rcvString]++(takeWhile (\a -> (a/= '+' && a /= '-')) (tail rcvString))) : parserPolynomial (dropWhile (\a -> (a/= '+' && a /= '-')) (tail rcvString))

-- Divide the terms of the polynomial into lists
termDivision:: String -> [String]
termDivision termString = if ( (head termString == '+') || (head termString == '-') )
                            then [head termString] : (takeWhile (\a -> (isDigit a || a == '.')) (dropWhile (\a -> not(isDigit a)) termString)) --take signal at front of string and take digits of term
                                : takeWhile (isLetter) (dropWhile (\a -> not(isLetter a) ) termString) -- take name of variable in the term
                                : [takeWhile (\a -> a /= ')') (dropWhile (\a -> not(isDigit a || a == '-')) (dropWhile (\a -> not(isLetter a)) termString))] -- take the exponent of the term
                            else ['+'] : (takeWhile (\a -> (isDigit a || a == '.')) (dropWhile (\a -> not(isDigit a)) termString)) --take signal at front of string and take digits of term
                                : takeWhile (isLetter) (dropWhile (\a -> not( isLetter a)) termString)
                                : [takeWhile (\a -> a /= ')') (dropWhile (\a -> not(isDigit a || a == '-')) (dropWhile (\a -> not(isLetter a)) termString))] -- take the exponent of the term
