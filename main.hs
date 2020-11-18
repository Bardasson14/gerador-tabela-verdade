import Data.List
import Data.Function
import Data.Maybe (fromJust)


type Letra = Char
data Formula = Var Letra
  | Conjuncao Formula Formula
  | Disjuncao Formula Formula
  | Negacao Formula Formula
  | Implicacao Formula Formula
  | Empty

main = do 
    putStrLn "Digite a fórmula: "
    formula <- getLine
    let parsedFormula = removeUnusedChars formula
    let finalParsedFormula = addExternalParenthesis parsedFormula
    let matchingParenthesisList = getMatchingParenthesis finalParsedFormula
    print(matchingParenthesisList)
    print(finalParsedFormula)
    let subFormulas = sliceSubFormulas finalParsedFormula matchingParenthesisList
    print subFormulas
    let varList = [[x]|x<-finalParsedFormula, x `elem` ['a'..'z'] || x `elem` ['A'..'Z']]
    let ordVarList = sort(varList)
    let _ordVarList = concat ordVarList --TRANSFORMA A LISTA DE VARIAVEIS EM STRING PARA DEPOIS TRANSFORMAR EM DUPLAS [(VARIAVEL,BOOL)]
    --let variaveis = listToPair _ordVarList
    --print variaveis
    let entries = nub ordVarList ++ subFormulas --CONSERTAR OPERADOR (->) NO CABEÇALHO
    --let res = 
    print entries

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

sliceSubFormulas :: [a] -> [(Int, Int)] -> [[a]]
sliceSubFormulas formula matchingParenthesis = [take ((snd x)-(fst x)-1) (drop (fst x+1) formula) | x <- matchingParenthesis]

--listToPair :: [Char] -> [(Char,Bool)]
--listToPair = map(\[a]->[(a,True)])

resolve :: Formula -> [(Letra, Bool)] -> Bool --RECEBE A SUBFÓRMULA COM AS LETRAS, ONDE CADA LETRA VAI TER O SEU VALOR E RETORNARÁ UM BOOL
resolve (Var a) xs = fromJust(lookup a xs) --RETORNA O VALOR QUE ESTÁ JUNTO COM A LETRA, NO CASO 0 OU 1
resolve (Conjuncao a b) xs = (resolve a xs) && (resolve b xs)
resolve (Disjuncao a b) xs = (resolve a xs) || (resolve b xs)
resolve (Implicacao a b) xs = not(resolve a xs) || (resolve b xs)
resolve (Negacao a Empty) xs = not(resolve a xs) 
 
--RESOLVER RECURSIVAMENTE AS FÓRMULAS A ESQUERDA E A DIREITA, P/CADA SUBFORMULA EM subFormulas

