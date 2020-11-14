
main = do 
    putStrLn "Digite a fórmula: "
    formula <- getLine
    let a = getMatchingParenthesis formula
    print(a)
    
splitFormula :: [a] -> [[a]]
splitFormula string = map (:[]) string

getParsedFormula :: [Char] -> [Char]
getParsedFormula formula = [c | c <- formula, c /= ' ', c /='>']

getMatchingParenthesis :: String -> [(Int, Int)]
getMatchingParenthesis = aux 0 []
  where
    aux _ _ [] = []
    aux currentIndex parenthesisStack ('(' : remainingString) = aux (currentIndex + 1) (currentIndex : parenthesisStack) remainingString
    aux currentIndex (lastOpenParenthesis:openParenthesis) (')' : remainingString) = (lastOpenParenthesis, currentIndex) : aux (currentIndex + 1) openParenthesis remainingString
    aux currentIndex parenthesisStack (c : remainingString) = aux (currentIndex + 1) parenthesisStack remainingString

--funcao para ir nos indices e substituir parenteses por n
--replaceString iterator counter
--replaceString  _ _ = []

