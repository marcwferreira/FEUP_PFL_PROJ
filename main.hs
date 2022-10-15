import Data.List
import Data.Char

-- Defining data and types for a polynomial

data Expo = Expo {var :: String , expoNum :: Float} deriving (Eq, Show)
data Term = Term {signal :: Char, numeric :: Float, expos :: [Expo]} deriving (Eq, Show)
type Poli = [Term]

-- AUX FUNCTIONS PARSER

-- remove char from string
removeOccurencesList:: Eq a => a -> [a] -> [a]
removeOccurencesList rcvItem rcvList = filter (/= rcvItem) rcvList


-- Checks is char recevied is a signal (+ or -)
isSignal:: Char -> Bool
isSignal rcvChar = rcvChar == '+' || rcvChar == '-'

-- Checks if char reveived is not a signal
notSignal:: Char -> Bool
notSignal rcvItem = not (isSignal rcvItem)

-- safe init
safeInit:: [a] -> [a]
safeInit [] = []
safeInit rcvList = init rcvList

-- Remove parenthesis surrouding number to facilitate other functions
surroudingParethesis:: String -> String
surroudingParethesis [] = []
surroudingParethesis rcvString = if (last rcvString) == ')'
                                    then  safeInit (dropWhileList isEqual rcvString "(")
                                    else dropWhileList isEqual rcvString "("

-- takes substring from string with characters that are accepted to be transformed into a float
takeWhileCharsFloat:: String -> String
takeWhileCharsFloat [] = []
takeWhileCharsFloat charString = takeWhile (\a-> (isDigit a) || a == '.' || a == '(' || a == ')') charString

-- check is a string received can be transformed into a float
checkFloatString:: String -> Bool
checkFloatString floatString = ((foldr (\x sum -> if x == '.' then sum+1 else sum) 0 fixedString) <= 1) -- can only have a max of one '.'
                                && ((foldr (\x sum -> if x == '(' || x == ')' then sum+1 else sum) 0 fixedString) == 0) -- check for parenthesis inside number
                                && (takeWhileCharsFloat (tail fixedString) == (tail fixedString)) -- can only have digits and '.', we don't check the first one since it is checked bellow
                                && (isDigit (last fixedString)) -- last element needs to be a number
                                && ((isDigit (head fixedString)) || ((isSignal (head fixedString)) && (isDigit ((!!) fixedString 1)))) -- first elements need to be a number or signal and number
                                where fixedString = surroudingParethesis floatString
                                
-- Transform a string in a float
getFloatFromString:: String -> Float 
getFloatFromString [] = 1
getFloatFromString formString = if (checkFloatString formString) --check if string can be transformed into float
                                then read formString :: Float
                                else error "invalid number in polinomyal" -- throw an arrow if it can't be transformed

-- Divides a string using + or - with these symbols at the beginning of the next one (not being used)
stringDivider:: String -> [String]
stringDivider [] = []
stringDivider rcvString = ([head rcvString]++(takeWhile (\a -> (a/= '+' && a /= '-')) (tail rcvString))) : stringDivider (dropWhile (\a -> (a/= '+' && a /= '-')) (tail rcvString))

-- Checks if two itens are equal
isEqual:: Eq a => a -> a -> Bool
isEqual iten1 iten2 = iten1 == iten2

-- drop while lists are equal
dropWhileList:: (a -> a -> Bool) -> [a] -> [a] -> [a]
dropWhileList _ [] [] = []
dropWhileList _ list1 [] = list1
dropWhileList _ [] list2 = []
dropWhileList cond list1 list2 = if cond (head list1) (head list2) 
                                    then [] ++ dropWhileList cond (tail list1) (tail list2)
                                    else list1

-- Check last subString part to see if next signal is a divide or not
verifyDivision:: String -> Bool
verifyDivision [] = True
verifyDivision rcvString = not (last rcvString == '(' || last rcvString == '^')

-- collects all substring until when it should actually be broken
takeUntilSignal:: String -> String
takeUntilSignal [] = []
takeUntilSignal rcvString = if verifyDivision (takeWhile notSignal (tail rcvString))
                            then firstSubString
                            else firstSubString ++ takeUntilSignal (dropWhileList isEqual rcvString firstSubString)
                            where 
                                firstSubString = [head rcvString]++(takeWhile notSignal (tail rcvString))

-- divide string into substring with the terms of the polynomial
smartStringDivider:: String -> [String]
smartStringDivider [] = []
smartStringDivider rcvString = firstString : smartStringDivider (dropWhileList isEqual rcvString firstString) where firstString = takeUntilSignal rcvString 

-- Splits a list using a as the divider
split:: Eq a => a -> [a] -> [[a]]
split _ [] = []
split delimiter rcvString = takeWhile (\a -> a /= delimiter) rcvString : split delimiter (dropWhile (\b -> b == delimiter) (dropWhile (\b -> b /= delimiter) rcvString))

-- PARSER

-- Creates the pair variable and exponent for each of the variables (individually)
expoCreation:: String -> Expo
expoCreation expoString = Expo (takeWhile (isLetter) (dropWhile (\a-> not (isLetter a)) expoString)) (getFloatFromString (drop 1 (dropWhile (\a-> a /= '^') expoString)))

-- Creates each term of the polynomial
termCreation:: String -> Term
termCreation [] = error "Term can not be empty"
termCreation termString = if isSignal (head termString)
                            then Term (head termString) (getFloatFromString (takeWhileCharsFloat (tail termString))) (map (expoCreation) (split '*' (dropWhile (\a-> not(isLetter a)) termString)))
                            else Term '+' (getFloatFromString (takeWhileCharsFloat termString)) (map (expoCreation) (split '*' (dropWhile (\a-> not(isLetter a)) termString)))

-- 4*x^2*y^2 + 2*(x*y)^2 + 2 new edge case

-- Creates the polynomial
poliCreation:: String -> [Term]
poliCreation stringList = map (termCreation) (smartStringDivider  (removeOccurencesList ' ' stringList))

-- PARSER END

-- AUX FUNCTIONS

-- function that compares the exponent of an "expo" to be used in sorting expos
expoGreaterNum:: Expo -> Expo -> Ordering
expoGreaterNum expo1 expo2 = if (expoNum expo1) >= (expoNum expo2) then LT else GT

-- function that compares the var of an "expo" to be used in sorting expos
expoGreaterVar:: Expo -> Expo -> Ordering
expoGreaterVar expo1 expo2 = if (var expo1) >= (var expo2) then GT else LT

-- function that compares the exponent of the highest "expo" to be used in sorting terms
termGreaterExponent:: Term -> Term -> Ordering
termGreaterExponent term1 term2 = if (expoNum (head (expos term1))) >= (expoNum (head (expos term2))) then LT else GT 

-- function that compares the var of the highest "expo" to be used in sorting terms
termGreaterVar:: Term -> Term -> Ordering
termGreaterVar term1 term2 = if (var (head (expos term1))) >= (var (head (expos term2))) then GT else LT 

-- function to verify that all the varuables have the same exponent
compareExpoNum:: Expo -> Expo -> Bool 
compareExpoNum expo1 expo2 = (expoNum expo1) == (expoNum expo2)

-- calculate the float using the signal and the numeric from a term
calculateFloatTerm:: Term -> Float
calculateFloatTerm term1 = if (signal term1) == '+'
                            then numeric term1
                            else -1 *(numeric term1)

-- function to sum terms if equals
sumTerms:: Term -> Term -> Term
sumTerms term1 term2 = if (expos term1) == (expos term2)
                        then Term '+' (calculateFloatTerm term1 + calculateFloatTerm term2) (expos term1)
                        else error "Can not sum terms with differents exponentials"

-- function to sum expos if equal
sumExpos:: Expo -> Expo -> Expo
sumExpos expo1 expo2 = if (var expo1) == (var expo2)
                        then Expo (var expo1) ((expoNum expo1)+(expoNum expo2))
                        else error "Can not sum expos with different variables"

-- function to remove expos with expoNum = 0
removeZeroExpos:: [Expo] -> [Expo]
removeZeroExpos [] = []
removeZeroExpos expoList = [x | x <- expoList, (expoNum x) /= 0]

-- NEXT FUNCTIONS

-- function to order expos first by exponent then by letter
sortExpos:: [Expo] -> [Expo]
sortExpos [] = []
sortExpos rcvList =  sortBy (expoGreaterVar) greaterExpoList ++ sortExpos (dropWhileList isEqual sortedByExpo greaterExpoList)
                        where sortedByExpo = sortBy (expoGreaterNum) rcvList
                              greaterExpoList = takeWhile (\a-> compareExpoNum (head sortedByExpo) a) sortedByExpo

-- function to sum all expos if they are equal
sumListExpos:: [Expo] -> [Expo]
sumListExpos [] = []
sumListExpos expoList = foldr (sumExpos) firstExpo [ x | x <- (tail expoList), (var firstExpo) == (var x)]
                        : sumListExpos [ x | x <- expoList, (var firstExpo) /= (var x)]
                        where firstExpo = head expoList


-- function to sum all equal terms from polynomial
sumListTerms:: [Term] -> [Term]
sumListTerms [] = []
sumListTerms termList = foldr (sumTerms) firstTerm [ x | x <- (tail termList), (expos firstTerm) == (expos x)]
                        : sumListTerms [ x | x <- termList, (expos firstTerm) /= (expos x)]
                        where firstTerm = head termList

-- function to order terms
sortTerms:: [Term] -> [Term]
sortTerms [] = []
sortTerms rcvList = sortBy (termGreaterVar) greaterTermList ++ sortTerms (dropWhileList isEqual sortedByTerm greaterTermList)
                        where sortedByTerm = sortBy (termGreaterExponent) rcvList
                              greaterTermList = takeWhile (\a-> compareExpoNum (head (expos  (head sortedByTerm))) ( head (expos a))) sortedByTerm

-- function to multiply numeric by expoNum then take 1 from expoNum
deriveTerm:: String ->Term -> Term
deriveTerm deriveVar term1 = if newNumeric >= 0
                                then Term '+'  newNumeric (Expo (var deriveExpo) ((expoNum deriveExpo)-1) : [x | x <- (expos term1) , (var x) /= deriveVar])
                                else Term '-'  (-1*newNumeric) (Expo (var deriveExpo) ((expoNum deriveExpo)-1) : [x | x <- (expos term1) , (var x) /= deriveVar])
                                where deriveExpo = (head (dropWhile (\a-> (var a) /= deriveVar) (expos term1)))
                                      newNumeric = (numeric term1) * (expoNum deriveExpo)

-- function to multiply terms (multiply all with all)
    -- implies functions to sum expos (their exponent)

-- function to print term

-- EXTRA WORK

-- parser with commands, for example:
    -- ORDER {polynomial}
    -- ADD {polynomial} {polynomial}
    -- MULTIPLY {polylonimal} {polynomial}
    -- DERIVE {variable} {polynomial}
-- OBS: {polynomial} can be another function like : ADD {MULTIPLY {polynomial} {polynomial}} {DERIVE {x} {ORDER {POLYNOMIAL}}}

-- EXTRA WORK END
