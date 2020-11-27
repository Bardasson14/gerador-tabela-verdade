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

--INPUT QUE CAUSOU ERRO: -> (&ab)(|(->cd)(|ef))
main = do 
    putStrLn "Digite a fórmula:"
    formula <- getLine
    let parsedFormula = removeUnusedChars formula
    let finalParsedFormula = addExternalParenthesis parsedFormula
    --GERAÇÃO DA TABELA VERDADE A PARTIR DA FINALPARSEDFORMULA
    let matchingParenthesisList = getMatchingParenthesis finalParsedFormula
    let subFormulas = sliceSubFormulas finalParsedFormula matchingParenthesisList
    let varList = [[x] |x<-finalParsedFormula, x `elem` ['a'..'z'] || x `elem` ['A'..'Z']]
    let truthTable = [binaryList(toBinary x) (length varList) | x <- [0..2^length (varList)-1]]
    let l = reverseList truthTable
    let parsedTT = [parseTTLine x | x<-l]
    let tt = [zip varList x | x<-parsedTT]
    let tt_header = varList ++ subFormulas
    let tt_vars = [beautifyTTLine (map snd x) | x<-tt]
    let subFormulaResults = [(beautifyTTLine (resolveLine x subFormulas)) | x <- tt]
    let final_tt = [stringifyTTLine(generateTTLine (tt_vars!!x) (subFormulaResults!!x)) |x<-[0..length(tt_vars)-1]]
    --print(map last tt_vars)
    let check = countT(map last subFormulaResults)
    print(stringifyTTLine tt_header)
    mapM_ print final_tt
    putStrLn("")
    putStrLn("")
    if (check==0) then print("Formula insatisfativel") else print("Formula satisfativel")
    if (check==length(tt_vars)) then print("Tautologia") else putStrLn("")


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

--PEGA SUBFORMULAS DE UMA FORMULA, IGORANDO PARENTESES ANINHADOS
stringToFormula :: String -> Formula
stringToFormula str |
 (((str!!0) == '&') && (length str == 3)) = Conjuncao (Var ([str!!1])) (Var ([str!!2]))  
 |(((str!!0) == '&') && ((length (getExternalSubFormulas str (getMatchingParenthesis str))) == 2)) = Conjuncao (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) ((stringToFormula (head(sliceSubFormulas str [last(getExternalSubFormulas str (getMatchingParenthesis str))])))) 
 |((str!!0 == '&')&&(fst(head(getExternalSubFormulas str (getMatchingParenthesis str)))) == 1) = Conjuncao (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) (Var ([str!!(snd(head(getExternalSubFormulas str (getMatchingParenthesis str)))+1)]))
 |(str!!0 == '&') = Conjuncao (Var [str!!1]) (stringToFormula (head(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))))) 
 |(((str!!0) == '|') && (length str == 3)) = Disjuncao (Var ([str!!1])) (Var ([str!!2]))  
 |(((str!!0) == '|') && ((length (getExternalSubFormulas str (getMatchingParenthesis str))) == 2)) = Disjuncao (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) ((stringToFormula (head(sliceSubFormulas str [last(getExternalSubFormulas str (getMatchingParenthesis str))])))) 
 |((str!!0 == '|') && (fst(head(getExternalSubFormulas str (getMatchingParenthesis str)))) == 1) = Disjuncao (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) (Var ([str!!(snd(head(getExternalSubFormulas str (getMatchingParenthesis str)))+1)]))
 |(str!!0 == '|') = Disjuncao (Var [str!!1]) (stringToFormula (head(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))))) 
 |(((str!!0) == '-') && (length str == 3)) = Implicacao (Var ([str!!1])) (Var ([str!!2]))  
 |(((str!!0) == '-') && ((length (getExternalSubFormulas str (getMatchingParenthesis str))) == 2)) = Implicacao (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) ((stringToFormula (head(sliceSubFormulas str [last(getExternalSubFormulas str (getMatchingParenthesis str))]))))
 |((str!!0 == '-')&&(fst(head(getExternalSubFormulas str (getMatchingParenthesis str)))) == 1) = Implicacao (stringToFormula (head(sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) (Var ([str!!(snd(head(getExternalSubFormulas str (getMatchingParenthesis str)))+1)]))
 |(str!!0 == '-') = Implicacao (Var [str!!1]) (stringToFormula (head(sliceSubFormulas str (getExternalSubFormulas str (getMatchingParenthesis str))))) 
 |(((str!!0) == '~') && ((length str == 2))) =  Negacao (Var[str!!1]) (Empty)
 |(str!!0 == '~') = Negacao (stringToFormula(head (sliceSubFormulas str [head(getExternalSubFormulas str (getMatchingParenthesis str))]))) (Empty)



generateNestedParenthesisList :: (Ord a1, Ord a2) => a1 -> a2 -> [(a1, a2)] -> [(a1, a2)]
generateNestedParenthesisList start end matchingParenthesis = [ x | x <- (sort matchingParenthesis), (fst x) > start, (snd x) < end]

allNested :: (Ord a1, Ord a2) => [(a1, a2)] -> [[(a1, a2)]]
allNested matchingParenthesis = [(generateNestedParenthesisList (fst x) (snd x) matchingParenthesis) | x<-matchingParenthesis] 
  
getInternalSubFormulas formula = nub (concat (filter (\x -> (length x /= 0)) (allNested (sort (getMatchingParenthesis formula)))))
--retorna subformulas em parenteses não aninhados (util p separar a string)

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

