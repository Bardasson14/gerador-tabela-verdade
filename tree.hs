import Data.List
import Data.Function
import Data.Maybe (fromJust)


type Letra = Char
data Formula = Var Letra
  | Conjuncao Formula Formula
  | Disjuncao Formula Formula
  | Negacao Formula
  | Implicacao Formula Formula

resolve :: Formula -> [(Letra, Bool)] -> Bool --RECEBE A SUBFÓRMULA COM AS LETRAS, ONDE CADA LETRA VAI TER O SEU VALOR E RETORNARÁ UM BOOL
resolve (Var a) xs = fromJust(lookup a xs) --RETORNA O VALOR QUE ESTÁ JUNTO COM A LETRA, NO CASO 0 OU 1
resolve (Conjuncao a b) xs = (resolve a xs) && (resolve b xs)
resolve (Disjuncao a b) xs = (resolve a xs) || (resolve b xs)
resolve (Implicacao a b) xs = not(resolve a xs) || (resolve b xs)
resolve (Negacao a) xs = not(resolve a xs) 

main = do
    let a = Formula A
    let formula = [Negacao a b]
    let aux = [(a,0),(b,1)]
    let valor = resolve formula aux
    print valor