
main = do 
    putStrLn "Digite a fórmula: "
    formula <- getLine
    let parsedFormula = removeUnusedChars formula
    --print parsedFormula 
    let finalParsedFormula = addExternalParenthesis parsedFormula
    let matchingParenthesisList = getMatchingParenthesis finalParsedFormula
    let subFormulas = createSubFormulasList matchingParenthesisList [] finalParsedFormula
    --let externalParenthesis = (0, length parsedFormula)
    --let finalParenthesisList = externalParenthesis:matchingParenthesisList
    print finalParsedFormula
    print matchingParenthesisList
    print subFormulas
--splitFormula :: [a] -> [[a]]
--splitFormula string = map (:[]) string

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

--NÃO INCLUIR FINAL NEM INICIO
sliceSubFormulas :: Int -> Int -> [a] -> [a]
sliceSubFormulas start end formula = take (end-start-1) (drop (start+1) formula)

createSubFormulasList :: [(Int, Int)] -> [[a]] -> [a] -> [[a]]
createSubFormulasList [] xs formula = []
createSubFormulasList [parenthesisList] xs formula = (sliceSubFormulas (fst parenthesisList) (snd parenthesisList) formula):xs