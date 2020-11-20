import Data.List
--import Data.Function
--import Data.Maybe (fromJust)

type Letra = Char
data Formula = Var Letra
  | Conjuncao Formula Formula
  | Disjuncao Formula Formula
  | Negacao Formula Formula
  | Implicacao Formula Formula
  | Empty

main = do 
    putStrLn "Digite a fórmula:"
    formula <- getLine
    let parsedFormula = removeUnusedChars formula
    let finalParsedFormula = addExternalParenthesis parsedFormula
    let matchingParenthesisList = getMatchingParenthesis finalParsedFormula
    --print(matchingParenthesisList)
    --print(finalParsedFormula)
    let subFormulas = sliceSubFormulas finalParsedFormula matchingParenthesisList
    --print subFormulas
    let varList = [[x]|x<-finalParsedFormula, x `elem` ['a'..'z'] || x `elem` ['A'..'Z']]
    let truthTable = [binaryList(toBinary x) (length varList) | x <- [0..2^(length (varList))-1]]
    let l = reverseList truthTable
    let parsedTT = [parseTTLine x | x<-l]
    let entries = nub varList ++ subFormulas --CONSERTAR OPERADOR (->) NO CABEÇALHO
    print entries
    print parsedTT

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

binaryList bin n = (replicate (n + 1 - length bin) 0) ++ cleanBinary(reverse bin)

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = mod n 2:toBinary(div n 2)
  
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

sliceSubFormulas :: [a] -> [(Int, Int)] -> [[a]]
sliceSubFormulas formula matchingParenthesis = [take ((snd x)-(fst x)-1) (drop (fst x+1) formula) | x <- matchingParenthesis]

resolve :: Formula -> [Bool] -> Bool --RECEBE A SUBFÓRMULA COM AS LETRAS, ONDE CADA LETRA VAI TER O SEU VALOR E RETORNARÁ UM BOOL
resolve (Conjuncao a b) xs = (resolve a xs) && (resolve b xs)
resolve (Disjuncao a b) xs = (resolve a xs) || (resolve b xs)
resolve (Implicacao a b) xs = not(resolve a xs) || (resolve b xs)
resolve (Negacao a Empty) xs = not(resolve a xs) 

parseTTLine line = [toBool x|x<-line]
toBool 0 = False
toBool 1 = True
operators = [(Implicacao, '-'), (Disjuncao, '|'), (Conjuncao, '&'), (Negacao, '~')] --OPERADORES SUPORTADOS
--RESOLVER RECURSIVAMENTE AS FÓRMULAS A ESQUERDA E A DIREITA, P/CADA SUBFORMULA EM subFormulas

-- generateAuxList :: [[Char]] -> [[Char]] -> [Bool] -> [Char] -> (String,Bool)
-- generateAuxList varList subFormulas truthTable x = 
