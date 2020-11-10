module BinaryTree where

--a é um parametro generico de tipo indefinido
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
               --NÓ --FILHO ESQ --FILHO DIR

--insertNode :: (Ord a) => a -> Tree a -> Tree a
insertNode :: a1 -> Tree a2 -> Tree a1
insertNode x EmptyTree = Node x EmptyTree EmptyTree
--insere o nó x, deixando os filhos vazios

--treeInsert x (Node a left right)
--    | x == a = Node x left right
--    | x <  a = Node a (treeInsert x left) right
--    | x >  a = Node a left (treeInsert x right)


--treeElem :: (Ord a) => a -> Tree a -> Bool
--treeElem x EmptyTree = False
--treeElem x (Node a left right)
--    | x == a = True
--    | x <  a = treeElem x left
--    | x >  a = treeElem x right