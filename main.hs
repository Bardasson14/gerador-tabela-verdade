import PropositionalLogic

main = do 
    putStrLn "Digite a fórmula:"
    formula <- getLine
    -- Formatação da fórmula, retirando caracteres inúteis e adicionando parênteses externos (se necessário)
    let parsedFormula = removeUnusedChars formula
    let finalParsedFormula = addExternalParenthesis parsedFormula
    -- Geração da lista de parênteses correspondentes, que permitirá dividir a fórmula em subfórmulas
    let matchingParenthesisList = getMatchingParenthesis finalParsedFormula
    let subFormulas = sliceSubFormulas finalParsedFormula matchingParenthesisList
    -- Geração da lista de variáveis presentes na fórmula
    let varList = [[x] |x<-finalParsedFormula, x `elem` ['a'..'z'] || x `elem` ['A'..'Z']]
    -- Cabeçalho da tabela verdade terá variáveis e subfórmulas
    let tt_header = varList ++ subFormulas
    -- Geração dos valores para a tabela verdade
    let truthTable = [binaryList(toBinary x) (length varList) | x <- [0..2^length (varList)-1]]
    let l = reverseList truthTable
    let parsedTT = [parseTTLine x | x<-l]
    let tt = [zip varList x | x<-parsedTT]
    -- Conversão dos valores binários para V e F
    let tt_vars = [beautifyTTLine (map snd x) | x<-tt]
    -- List comprehension para resolver cada subfórmula de acordo com o valor correspondente na Tabela Verdade
    let subFormulaResults = [(beautifyTTLine (resolveLine x subFormulas)) | x <- tt]
    let final_tt = [stringifyTTLine(generateTTLine (tt_vars!!x) (subFormulaResults!!x)) |x<-[0..length(tt_vars)-1]]
    
    print(stringifyTTLine tt_header)
    mapM_ print final_tt

    -- Checagem de satisfabilidade e tautologia
    let check = countT(map last subFormulaResults)
    putStrLn("")
    putStrLn("")
    if (check==0) then print("Formula insatisfativel") else print("Formula satisfativel")
    if (check==length(tt_vars)) then print("Tautologia") else putStrLn("")




