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
    --let binTree = Node (Empty) 'a' (Empty) --folha 5
    --let binTree2 = Node (Node Empty "a" Empty) "->" (Node Empty "b" Empty)
    let tree = Empty
    let tree2 = (treeInsert 'a' tree 0)
    let tree3 = (treeInsert 'b' tree2 2)

    print $tree2
    print $tree3
    --let binTree2 = Node()
    --print $binTree
    --print $binTree2

getVariableString :: [Char] -> [Char]
getVariableString formula = [c | c <- formula, c `elem` ['a'..'z']]

splitVariableString :: [a] -> [[a]]
splitVariableString string = map (:[]) string

getParsedFormula :: [Char] -> [Char]
getParsedFormula formula = [c | c <- formula, c /= ' ']

splitParsedFormula :: [a] -> [[a]]
splitParsedFormula parsedFormula = map (:[]) parsedFormula

--getSubformulas :: [a] -> [a]
--getSubformulas splittedFormula = [c | c <- splittedFormula]

--a^(b&(c->d))

--a é um parametro generico de tipo indefinido
data Tree a =
            Empty | Node (Tree a) a (Tree a) deriving (Show) 
             --FILHO ESQ NÓ --FILHO DIR
        
singleton :: a -> Tree a
singleton x = Node Empty x Empty -- atalho para criar uma folha (nó com informação, porém sem filhos)

--Múltiplas definições para a função treeInsert
--Pattern Matching
--1o parametro: nó a ser inserido
--2o parametro: nó onde x será inserido
--3o parametro: onde inserir? (0-ind, 1-esq, 2-dir)

treeInsert :: (Eq t, Num t) => a -> Tree a -> t -> Tree a
treeInsert x Empty 0 = singleton x
treeInsert x (Node left a right) 1 = Node (singleton x) a right 
treeInsert x (Node left a right) 2 = Node left a (singleton x) 

--Se for passado Empty como parametro, a árvore assumirá a forma do singleton x (uma folha)
--treeInsert x "none" (Node left a right) = singleton x --Quando a árvore passada for vazia, retornar apenas o singleton (folha)
--treeInsert x "left" (Node left a right) = Node (treeInsert x left) a --right

operators = ["|", "&", "->", "<->", "¬"]

--PRÓXIMO PASSO: APLICAR MAP