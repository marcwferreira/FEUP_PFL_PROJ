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

-- Checks is char recevied is a signal
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

-- Divides a string using + or - with these symbols at the beginning of the next one
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

-- Creates the polynomial
poliCreation:: String -> [Term]
poliCreation stringList = map (termCreation) (smartStringDivider  (removeOccurencesList ' ' stringList))

-- PARSER END

-- AUX FUNCTIONS

-- function that compares the exponent of an "expo" to be used in a sort function
expoGreaterNum:: Expo -> Expo -> Ordering
expoGreaterNum expo1 expo2 = if (expoNum expo1) >= (expoNum expo2) then LT else GT

-- function that compares the var of an "expo" to be used in a sort function
expoGreaterVar:: Expo -> Expo -> Ordering
expoGreaterVar expo1 expo2 = if (var expo1) >= (var expo2) then GT else LT

-- function to verify that al the varuables have the same exponent
compareExpoNum:: Expo -> Expo -> Bool 
compareExpoNum expo1 expo2 = (expoNum expo1) == (expoNum expo2)


-- NEXT FUNCTIONS

-- function to order expos first by exponent then by letter
sortExpos:: [Expo] -> [Expo]
sortExpos rcvList =  (head sortedByExpo) : sortBy (expoGreaterVar) (takeWhile (\a-> compareExpoNum (head sortedByExpo) a) (tail sortedByExpo))
                        where sortedByExpo = sortBy (expoGreaterNum) rcvList

-- function to see if expos are equal (after ordering)

-- function so sum expos if equal

-- function to order terms

-- function to sum terms if equals

-- function to multiply numeric by expoNum then take 1 from expoNum

-- function to multiply terms (multiply all with all)
    -- implies functions to sum expos (their exponent)

-- function to print term

-- EXTRA WORK

-- parser with commands, for example:
    -- ORDER {polynomial}
    -- ADD {polynomial} {polynomial}
    -- MULTIPLY {polylonimal} {polynomial}
    -- DERIVE {polynomial}
-- OBS: {polynomial} can be another function like : ADD {MULTIPLY {polynomial} {polynomial}} {DERIVE {ORDER {POLYNOMIAL}}}

-- EXTRA WORK END


--backup in case I screwed something up (this has a bug)
{-
-- divide string into substring with the terms of the polynomial
smartStringDivider:: String -> [String]
smartStringDivider [] = []
smartStringDivider rcvString = if verifyDivision (takeWhile notSignal (tail rcvString)) --use tail because first char might be a signal
                                then firstSubString : smartStringDivider (dropWhileList isEqual rcvString firstSubString)
                                else (firstSubString ++ [head remainderString] ++ (takeWhileCharsFloat (tail remainderString))) -- forgot divide with * fix this!!!!!!
                                    : smartStringDivider (dropWhileList isEqual rcvString (firstSubString ++ secondSubString))
                                where   
                                    firstSubString = ([head rcvString]++(takeWhile notSignal (tail rcvString)))
                                    remainderString = (dropWhileList isEqual rcvString firstSubString)
                                    secondSubString = (head remainderString) : (takeWhileCharsFloat (tail remainderString))
-}
