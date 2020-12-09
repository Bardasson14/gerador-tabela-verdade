
module PropositionalLogic (Formula, generateTTLine, beautifyTTLine, stringifyTTLine, resolveLine, getValue, countT, stringToFormula, generateNestedParenthesisList, allNested, getInternalSubFormulas, getExternalSubFormulas, removeUnusedChars, getMatchingParenthesis, addExternalParenthesis, toBinary, cleanBinary, reverseList, sliceSubFormulas, resolve, parseTTLine, toBool, binaryList) where

import Data.Maybe (fromJust)
import Data.List ( intercalate, nub, sort )

data Formula =
  Var String | Val Bool 
  | And Formula Formula
  | Or Formula Formula
  | Implication Formula Formula
  | Not Formula Formula
  | Empty
  
generateTTLine :: [[Char]] -> [[Char]] -> [[Char]]
generateTTLine tt_vars tt_subformulas = tt_vars ++ tt_subformulas

beautifyTTLine tt_line = [if x then "T" else "F" |x<-tt_line]

stringifyTTLine :: [[Char]] -> [Char]
stringifyTTLine  = intercalate " || "

resolveLine :: [([Char], Bool)] -> [String] -> [Bool]
resolveLine tt_line subFormulas = [resolve (stringToFormula x) tt_line | x<-subFormulas]
    
--ASSOCIA VALORES BOOLEANOS ÀS VARIÁVEIS
getValue :: [Char] -> [([Char], Bool)] -> Bool
getValue str tt =  fromJust(lookup str tt)

countT l = if length (filter (\x -> (x=="T")) l) > 0 then length (filter (\x -> (x=="T")) l) else 0


stringToFormula :: String -> Formula
stringToFormula str |
 (((str!!0) == '&') && (length str == 3)) = And (Var ([str!!1])) (Var ([str!!2]))  
 |(((str!!0) == '&') && ((length (getExternalSubFormulas str (getMatchingParenthesis str))) == 2)) = And (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) ((stringToFormula (head(sliceSubFormulas str [last(getExternalSubFormulas str (getMatchingParenthesis str))])))) 
 |((str!!0 == '&')&&(fst(head(getExternalSubFormulas str (getMatchingParenthesis str)))) == 1) = And (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) (Var ([str!!(snd(head(getExternalSubFormulas str (getMatchingParenthesis str)))+1)]))
 |(str!!0 == '&') = And (Var [str!!1]) (stringToFormula (head(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))))) 
 |(((str!!0) == '|') && (length str == 3)) = Or (Var ([str!!1])) (Var ([str!!2]))  
 |(((str!!0) == '|') && ((length (getExternalSubFormulas str (getMatchingParenthesis str))) == 2)) = Or (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) ((stringToFormula (head(sliceSubFormulas str [last(getExternalSubFormulas str (getMatchingParenthesis str))])))) 
 |((str!!0 == '|') && (fst(head(getExternalSubFormulas str (getMatchingParenthesis str)))) == 1) = Or (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) (Var ([str!!(snd(head(getExternalSubFormulas str (getMatchingParenthesis str)))+1)]))
 |(str!!0 == '|') = Or (Var [str!!1]) (stringToFormula (head(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))))) 
 |(((str!!0) == '-') && (length str == 3)) = Implication (Var ([str!!1])) (Var ([str!!2]))  
 |(((str!!0) == '-') && ((length (getExternalSubFormulas str (getMatchingParenthesis str))) == 2)) = Implication (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) ((stringToFormula (head(sliceSubFormulas str [last(getExternalSubFormulas str (getMatchingParenthesis str))]))))
 |((str!!0 == '-')&&(fst(head(getExternalSubFormulas str (getMatchingParenthesis str)))) == 1) = Implication (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) (Var ([str!!(snd(head(getExternalSubFormulas str (getMatchingParenthesis str)))+1)]))
 |(str!!0 == '-') = Implication (Var [str!!1]) (stringToFormula (head(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))))) 
 |(((str!!0) == '~') && ((length str == 2))) =  Not (Var[str!!1]) (Empty)
 |(str!!0 == '~') = Not (stringToFormula(head (sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) (Empty)



generateNestedParenthesisList :: (Ord a1, Ord a2) => a1 -> a2 -> [(a1, a2)] -> [(a1, a2)]
generateNestedParenthesisList start end matchingParenthesis = [ x | x <- (sort matchingParenthesis), (fst x) > start, (snd x) < end]

allNested :: (Ord a1, Ord a2) => [(a1, a2)] -> [[(a1, a2)]]
allNested matchingParenthesis = [(generateNestedParenthesisList (fst x) (snd x) matchingParenthesis) | x<-matchingParenthesis] 
  
getInternalSubFormulas formula = nub (concat (filter (\x -> (length x /= 0)) (allNested (sort (getMatchingParenthesis formula)))))
--retorna subformulas em parenteses não aninhados (util p separar a string)

getExternalSubFormulas :: String -> [(Int, Int)] -> [(Int, Int)]
getExternalSubFormulas formula matchingParenthesis= [x|x<-matchingParenthesis, x `notElem` getInternalSubFormulas formula]

removeUnusedChars :: [Char] -> [Char]
removeUnusedChars formula = [c | c <- formula, c /= ' ', c /='>']

getMatchingParenthesis :: String -> [(Int, Int)]
getMatchingParenthesis = aux 0 []
  where
    aux _ _ [] = []
    aux currentIndex parenthesisStack ('(' : remainingString) = aux (currentIndex + 1) (currentIndex : parenthesisStack) remainingString
    aux currentIndex (lastOpenParenthesis:openParenthesis) (')' : remainingString) = (lastOpenParenthesis, currentIndex) : aux (currentIndex + 1) openParenthesis remainingString
    aux currentIndex parenthesisStack (c : remainingString) = aux (currentIndex + 1) parenthesisStack remainingString

addExternalParenthesis :: String -> String
addExternalParenthesis [] = []
addExternalParenthesis (x:xs) = if x/='(' then "("++x:xs++")" else x:xs

cleanBinary (x:xs) = if x == 0 then xs else x:xs

binaryList :: (Num a, Eq a) => [a] -> Int -> [a]
binaryList bin n = (replicate (n + 1 - length bin) 0) ++ cleanBinary(reverse bin)

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = mod n 2:toBinary(div n 2)
  
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

sliceSubFormulas :: [a] -> [(Int, Int)] -> [[a]]
sliceSubFormulas formula matchingParenthesis = [take ((snd x)-(fst x)-1) (drop (fst x+1) formula) | x <- matchingParenthesis]

resolve :: Formula -> [([Char],Bool)] -> Bool  --a corrigir
resolve (Var v) bs      = fromJust(lookup v bs)
resolve (And a b) xs = (resolve a xs) && (resolve b xs)
resolve (Or a b) xs = (resolve a xs) || (resolve b xs)
resolve (Implication a b) xs = not(resolve a xs) || (resolve b xs)
resolve (Not a Empty) xs = not(resolve a xs) 

parseTTLine line = [toBool x|x<-line]
toBool 0 = False
toBool 1 = True