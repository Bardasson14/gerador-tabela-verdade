
main = do 
    putStrLn "Digite a fórmula: ";
    formula <- getLine;
    putStrLn formula;
    let variableString = getVariableString formula;
    let variablesList = splitVariableString variableString
    let parsedFormulaString = getParsedFormula formula;
    let subFormulas = splitParsedFormula parsedFormulaString;
    print $subFormulas;
    

getVariableString formula = [c | c <- formula, c `elem` ['a'..'z']];
splitVariableString string = map (:[]) string;

getParsedFormula formula = [c | c <- formula, c /= ' '];
splitParsedFormula parsedFormula = map (:[]) parsedFormula;

getSubformulas splittedFormula = [c | c <- splittedFormula];

--a^(b&(c->d))