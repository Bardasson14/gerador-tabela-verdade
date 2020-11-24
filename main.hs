import Data.List
--import Data.Function
import Data.Maybe
--import Data.Typeable

type Letra = Char
--data Operation = Conjuncao | Disjuncao | Implicacao | Negacao deriving Enum
data Formula =
  Var String | Val Bool 
  | Conjuncao Formula Formula
  | Disjuncao Formula Formula
  | Implicacao Formula Formula
  | Negacao Formula Formula
  | Empty
  deriving Show
  
main = do 
    putStrLn "Digite a fórmula:"
    formula <- getLine
    let parsedFormula = removeUnusedChars formula
    --let finalParsedFormula = addExternalParenthesis parsedFormula
    let finalParsedFormula = parsedFormula
    let matchingParenthesisList = getMatchingParenthesis finalParsedFormula
    let subFormulas = sliceSubFormulas finalParsedFormula matchingParenthesisList
    let varList = [[x] |x<-finalParsedFormula, x `elem` ['a'..'z'] || x `elem` ['A'..'Z']]
    
    let truthTable = [binaryList(toBinary x) (length varList) | x <- [0..2^length (varList)-1]]
    let l = reverseList truthTable
    let parsedTT = [parseTTLine x | x<-l]
    let tt = [zip varList x | x<-parsedTT] 

    let m = filter (\x -> (length x /= 0)) (allNested (sort matchingParenthesisList))
    let nestedParenthesis = nub (concat m)
    print nestedParenthesis
    let subformulasP = [x|x<- matchingParenthesisList, x `notElem` nestedParenthesis] 
    --TÁ FUNCIONANDO 
    --let test = [resolve(stringToFormula (finalParsedFormula!!0) finalParsedFormula x) | x<-tt]
    --let a = stringToFormula (finalParsedFormula!!1) finalParsedFormula
    --let solved = [resolve(stringToFormula (finalParsedFormula!!1) finalParsedFormula) (x)|x<-tt]
    --print solved
    --print subformulasP
    --print (stringToFormula (finalParsedFormula!!0) finalParsedFormula (length subformulasP) (subformulasP))
    print tt
    let r = [resolve(stringToFormula (finalParsedFormula!!0) finalParsedFormula (length subformulasP) (subformulasP)) x | x<-tt]
    print r
    print subFormulas
    
--botar variaveis na tabela verdade
getValue :: [Char] -> [([Char], Bool)] -> Bool
getValue str tt =  fromJust(lookup str tt)

--AINDA ESTÁ LIDANDO APENAS COM FÓRMULAS SIMPLES
--USAR SLICE SUBFORMULAS
--PARAMETROS: OPERADOR STRING E NUMERO DE SUBFORMULAS CONTIDAS
stringToFormula :: Char -> [Char] -> Int -> [(Int,Int)]-> Formula
stringToFormula '&' str 0 [] = Conjuncao (Var ([str!!1])) (Var ([str!!2]))
stringToFormula '-' str 0 [] = Implicacao (Var ([str!!1])) (Var ([str!!2]))
stringToFormula '|' str 0 [] = Disjuncao (Var ([str!!1])) (Var ([str!!2]))
stringToFormula '~' str 0 [] = Negacao (Var([str!!1])) Empty
--fazer p/ 1 esq dir
stringToFormula '&' str 2 (x:xs) = Conjuncao (Var (head (sliceSubFormulas str [x]))) (Var(last(sliceSubFormulas str xs)))
stringToFormula '-' str 2 (x:xs) = Implicacao (Var (head (sliceSubFormulas str [x]))) (Var(last(sliceSubFormulas str xs)))
stringToFormula '|' str 2 (x:xs) = Disjuncao (Var (head (sliceSubFormulas str [x]))) (Var (last(sliceSubFormulas str xs)))

generateNestedParenthesisList :: (Ord a1, Ord a2) => a1 -> a2 -> [(a1, a2)] -> [(a1, a2)]
generateNestedParenthesisList start end matchingParenthesis = [ x | x <- (sort matchingParenthesis), (fst x) > start, (snd x) < end]

allNested :: (Ord a1, Ord a2) => [(a1, a2)] -> [[(a1, a2)]]
allNested matchingParenthesis = [(generateNestedParenthesisList (fst x) (snd x) matchingParenthesis) | x<-matchingParenthesis] 

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

resolve :: Formula -> [([Char],Bool)] -> Bool  --a corrigir
resolve (Var v) bs      = fromJust(lookup v bs)
resolve (Conjuncao a b) xs = (resolve a xs) && (resolve b xs)
resolve (Disjuncao a b) xs = (resolve a xs) || (resolve b xs)
resolve (Implicacao a b) xs = not(resolve a xs) || (resolve b xs)
resolve (Negacao a Empty) xs = not(resolve a xs) 

parseTTLine line = [toBool x|x<-line]
toBool 0 = False
toBool 1 = True
--operators = [(Implicacao,'-'), (Disjuncao,'|'), (Conjuncao,'&'), (Negacao,'~')] --OPERADORES SUPORTADOS

