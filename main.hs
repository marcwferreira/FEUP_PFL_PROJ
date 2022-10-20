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

-- Creates the polynomial
poliCreation:: String -> Poli
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
termGreaterExponent term1 term2 = if (expoNum (getFirstExpo (expos term1))) >= (expoNum (getFirstExpo (expos term2))) then LT else GT 

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
                        then if numbersSum >= 0
                            then Term '+' numbersSum (expos term1)
                            else Term '-' (-1*numbersSum) (expos term1)
                        else error "Can not sum terms with differents exponentials"
                        where numbersSum = calculateFloatTerm term1 + calculateFloatTerm term2

-- function to sum expos if equal
sumExpos:: Expo -> Expo -> Expo
sumExpos expo1 expo2 = if (var expo1) == (var expo2)
                        then Expo (var expo1) ((expoNum expo1)+(expoNum expo2))
                        else error "Can not sum expos with different variables"

-- function to remove expos with expoNum = 0
removeZeroExpos:: [Expo] -> [Expo]
removeZeroExpos [] = []
removeZeroExpos expoList = [x | x <- expoList, (expoNum x) /= 0]

-- function to remove zeros exponents within a term
removeZeroTerm:: Term -> Term
removeZeroTerm rcvTerm = Term (signal rcvTerm) (numeric rcvTerm) (removeZeroExpos (expos rcvTerm))

-- function remove terms with numeric as 0
removeZeroPoli:: Poli -> Poli
removeZeroPoli [] = []
removeZeroPoli rcvPoli = [ x | x<- rcvPoli, (numeric x) /= 0]

-- Funtion to get float with signal from Term
termFloatSignal:: Term -> Float
termFloatSignal rcvTerm = if (signal rcvTerm) == '+' then (numeric rcvTerm) else (-1*(numeric rcvTerm))

-- funtion to get the first exponent of a list of expos, returns 0 if there is none
getFirstExpo:: [Expo] -> Expo
getFirstExpo [] = Expo "null" 0
getFirstExpo rcvList = head rcvList

-- auxiliary funtion to help the sort by length of expos
termExposLength:: Term -> Term -> Ordering
termExposLength term1 term2 = if (length (expos term1)) >= (length (expos term2)) then LT else GT


-- function to sort terms by length of expos, used after getting terms with same highest exponent
sortTermsByLength:: Poli -> Poli
sortTermsByLength [] = []
sortTermsByLength rcvPoli = sortBy (termExposLength) rcvPoli
                        
-- NEXT FUNCTIONS

-- function to order expos first by letter then by exponent
sortExpos:: [Expo] -> [Expo]
sortExpos [] = []
sortExpos rcvList = sortBy (expoGreaterNum) orderVarList ++ sortExpos (dropWhileList isEqual sortedByVar orderVarList)
                    where sortedByVar = sortBy (expoGreaterVar) rcvList
                          orderVarList = takeWhile (\a -> (var a) == (var (head sortedByVar))) sortedByVar

-- function to sort expos of a term
sortTermExpos:: Term -> Term
sortTermExpos rcvTerm = Term (signal rcvTerm) (numeric rcvTerm) (sortExpos (expos rcvTerm))

-- function to order terms
sortTerms:: [Term] -> [Term]
sortTerms [] = []
sortTerms rcvList = sortTermsByLength (sortBy (termGreaterVar) greaterTermList) ++ sortTerms (dropWhileList isEqual sortedByTerm greaterTermList)
                    where sortedByTerm = sortBy (termGreaterExponent) rcvList
                          greaterTermList = takeWhile (\a-> compareExpoNum (getFirstExpo (expos  (head sortedByTerm))) ( getFirstExpo (expos a))) sortedByTerm

-- function to sum all expos if they are equal
sumListExpos:: [Expo] -> [Expo]
sumListExpos [] = []
sumListExpos expoList = foldr (sumExpos) firstExpo [ x | x <- (tail expoList), (var firstExpo) == (var x)]
                        : sumListExpos [ x | x <- expoList, (var firstExpo) /= (var x)]
                        where firstExpo = head expoList

-- function to sum expos of a term if they are equal
sumTermExpos:: Term -> Term
sumTermExpos rcvTerm = Term (signal rcvTerm) (numeric rcvTerm) (sumListExpos (expos rcvTerm))


-- function to sum all equal terms from polynomial
sumListTerms:: [Term] -> [Term]
sumListTerms [] = []
sumListTerms termList = foldr (sumTerms) firstTerm [ x | x <- (tail termList), (expos firstTerm) == (expos x)]
                        : sumListTerms [ x | x <- termList, (expos firstTerm) /= (expos x)]
                        where firstTerm = head termList

-- function to multiply numeric by expoNum then take 1 from expoNum (derivative)
deriveTerm:: String -> Term -> Term
deriveTerm deriveVar term1 = if length (expos term1) == 0
                                then Term '+' 0 []
                                else
                                    if newNumeric >= 0
                                    then Term '+'  newNumeric (Expo (var deriveExpo) ((expoNum deriveExpo)-1) : nonDerivingTerms)
                                    else Term '-'  (-1*newNumeric) (Expo (var deriveExpo) ((expoNum deriveExpo)-1) : nonDerivingTerms)
                                    where deriveExpo = (head (dropWhile (\a-> (var a) /= deriveVar) (expos term1)))
                                          newNumeric = (termFloatSignal term1) * (expoNum deriveExpo)
                                          nonDerivingTerms = [x | x <- (expos term1) , (var x) /= deriveVar]

-- function to derive 

-- function to multiply terms (multiply all with all)
multiplyTerm:: Term -> Term -> Term
multiplyTerm term1 term2 = if newNumeric >= 0
                            then Term '+' newNumeric (sumListExpos ((expos term1) ++ (expos term2)))
                            else Term '-' (-1*newNumeric) (sumListExpos ((expos term1) ++ (expos term2)))
                            where newNumeric = (termFloatSignal term1) * (termFloatSignal term2)

--function to multiply all terms
multiplyPolis:: Poli -> Poli -> Poli
multiplyPolis [] _ = []
multiplyPolis _ [] = []
multiplyPolis poli1 poli2 = [ (multiplyTerm x y) | x <- poli1, y <- poli2]


-- AUX PRINT FUNCTIONS

-- function to join list of strings with divider
joinStrings:: Char -> [String] -> String
joinStrings _ []  = ""
joinStrings divider (x:xs) = foldl' (\a b -> a ++ [divider] ++ b) x xs

-- function to transform floats into string depending on their value
floatToString:: Float -> String
floatToString rcvFloat = if (rcvFloat - (fromIntegral (round rcvFloat) :: Float)) < 0.001 && ((fromIntegral (round rcvFloat) :: Float) - rcvFloat) < 0.001
                         then (show (round rcvFloat :: Int))
                         else if (last approxNum == '.') then (safeInit approxNum) else approxNum
                         where rcvNum = take 5 (show rcvFloat)
                               approxNum = if (any (\x -> x == (last rcvNum))  ['5','6'..'9'])
                                           then reverse (dropWhile (isEqual '0') (reverse (take 4 (show (rcvFloat+0.01)))))
                                           else reverse (dropWhile (isEqual '0') (reverse (safeInit rcvNum)))

-- function to transform expo into string
expoToString:: Expo -> String
expoToString rcvExpo = if (expoNum rcvExpo) > 0 
                        then if (expoNum rcvExpo) == 1
                            then var rcvExpo
                            else (var rcvExpo) ++ "^" ++ (floatToString (expoNum rcvExpo))
                        else (var rcvExpo) ++ "^(" ++ (floatToString (expoNum rcvExpo)) ++ ")"

-- function to transform term into string
termToString:: Term -> String
termToString rcvTerm = (signal rcvTerm) : ' ' : (floatToString (numeric rcvTerm)) ++ (joinStrings '*' (map (expoToString) (expos rcvTerm)))

-- function to print first theme without signal if number is positive
firstTermToString:: Term -> String
firstTermToString rcvTerm = if ((signal rcvTerm) == '+')
                            then (floatToString (numeric rcvTerm)) ++ (joinStrings '*' (map (expoToString) (expos rcvTerm)))
                            else '-' : (floatToString (numeric rcvTerm)) ++ (joinStrings '*' (map (expoToString) (expos rcvTerm)))
--function to transform poli into string
poliToString:: Poli -> String
poliToString [] = "0"
poliToString rcvPoli = firstTermToString (head rcvPoli) ++ " " ++ (joinStrings ' ' (map (termToString) (tail rcvPoli)))

-- MAIN FUNCTIONS

-- function to normalize the polynomial
normPoli:: String -> String
normPoli [] = []
normPoli rcvString = poliToString (sortTerms (removeZeroPoli (map removeZeroTerm (map (sumTermExpos) (map (sortTermExpos) (sumListTerms (map removeZeroTerm (map (sumTermExpos) (map (sortTermExpos) (map removeZeroTerm (poliCreation rcvString)))))))))))

-- function to add polynomials
addPolis:: String -> String -> String
addPolis [] poli2 = normPoli poli2
addPolis poli1 [] = normPoli poli1
addPolis poli1 poli2 = normPoli (poliToString (sumListTerms (poliCreation (normPoli poli1)) ++ (poliCreation (normPoli poli2))))

-- function to multiply polynomial
multPolis:: String -> String -> String
multPolis _ [] = "can't multiply with empty polynomial"
multPolis [] _ = "can't multiply with empty polynomial"
multPolis poli1 poli2 = normPoli (poliToString (multiplyPolis (poliCreation (normPoli poli1)) (poliCreation (normPoli poli2))))

-- function to derivate a terminal
derivPoli:: String -> String -> String
derivPoli _ [] = []
derivPoli deriveVar rcvPoli = normPoli (poliToString (map (deriveTerm deriveVar) (poliCreation (normPoli rcvPoli))))
 
--TODO
-- fix sorting of terms - actually first by letter than by highest exponent (then by total power)
-- generate tests (readme)