import Data.List
main = do 
    putStrLn "Digite a fórmula: "
    formula <- getLine
    let parsedFormula = removeUnusedChars formula
    let finalParsedFormula = addExternalParenthesis parsedFormula
    let matchingParenthesisList = getMatchingParenthesis finalParsedFormula
    let subFormulas = sliceSubFormulas finalParsedFormula matchingParenthesisList
    let varList = [[x]|x<-finalParsedFormula, x `elem` ['a'..'z'] || x `elem` ['A'..'Z']]
    let entries = nub varList ++ subFormulas --CONSERTAR OPERADOR (->) NO CABEÇALHO
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
addExternalParenthesis (x:xs) = "("++x:xs++")"

sliceSubFormulas :: [a] -> [(Int, Int)] -> [[a]]
sliceSubFormulas formula matchingParenthesis = [take ((snd x)-(fst x)-1) (drop (fst x+1) formula) | x <- matchingParenthesis]

operators = ["-", "|", "&", "~"] --OPERADORES SUPORTADOS
--RESOLVER RECURSIVAMENTE AS FÓRMULAS A ESQUERDA E A DIREITA, P/CADA SUBFORMULA EM subFormulas
