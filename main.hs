--AINDA É NECESSÁRIO SEPARAR OS MÓDULOS

main = do 
    putStrLn "Digite a fórmula: "
    formula <- getLine
    putStrLn formula
    --let variableString = getVariableString formula
    --let variablesList = splitVariableString variableString
    let parsedFormulaString = getParsedFormula formula
    let subFormulas = splitParsedFormula parsedFormulaString
    print $subFormulas
    let binTree = Node (Empty) 'a' (Empty) --folha 5
    let binTree2 = Node (Node(Empty) 'a' (Empty)) '>' (Node (Empty) 'b' (Empty))
    --let binTree2 = Node()
    print $binTree
    print $binTree2

getVariableString :: [Char] -> [Char]
getVariableString formula = [c | c <- formula, c `elem` ['a'..'z']]

splitVariableString :: [a] -> [[a]]
splitVariableString string = map (:[]) string

getParsedFormula :: [Char] -> [Char]
getParsedFormula formula = [c | c <- formula, c /= ' ']

splitParsedFormula :: [a] -> [[a]]
splitParsedFormula parsedFormula = map (:[]) parsedFormula

getSubformulas :: [a] -> [a]
getSubformulas splittedFormula = [c | c <- splittedFormula]

--a^(b&(c->d))

--a é um parametro generico de tipo indefinido
data Tree a =
            Empty | Node (Tree a) a (Tree a) deriving (Show) 
             --FILHO ESQ NÓ --FILHO DIR
        

--insertNode :: (Ord a) => a -> Tree a -> Tree a
--insertNode x EmptyTree = Node x EmptyTree EmptyTree

--insere o nó x, deixando os filhos vazios
--treeInsert x (Node a left right)
--    | x == a = Node x left right
--    | x <  a = Node a (treeInsert x left) right
--    | x >  a = Node a left (treeInsert x right)