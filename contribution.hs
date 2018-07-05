--Material base: https://sites.google.com/a/cin.ufpe.br/if686ec/aulas
--Sugestao de estudo complementar:
--http://www.facom.ufu.br/~madriana/PF/ 
--https://www.udemy.com/curso-haskell/
--http://www.decom.ufop.br/lucilia/func/slides/
--http://www.dcc.fc.up.pt/~pbv/aulas/pf/
--http://homepages.dcc.ufmg.br/~camarao/haskell/
--http://www.inf.ufpr.br/andrey/ci062/ProgramacaoHaskell.pdf
--http://www.decom.ufop.br/romildo/2014-2/bcc222/practices/progfunc.pdf

-- ##############################################################################################################################################################
 {- Exercícios em Haskell -}
 
 1){- Faça uma função que recebe duas listas de inteiros e compare se elas são iguais, se sim, imprimir TRUE, se não, imprimir FALSE-}
 
 listaigual :: => [Int] -> [Int] -> Bool
 listaigual [] [] =  (coisa == asdodsa)
 listaigual [] _ = False
 listaigual _ [] = False
 listaigual (a:bc) (d:ed) = | a==d = listaigual (bc) (ed)
                            |otherwise = False
-- ##############################################################################################################################################################

 2) {-Faça uma funcao que recebe tres listas de char e compare se elas sao iguais, retornando true ou false-}

listachar :: [Char] -> [Char] -> [Char] -> Bool
listachar [] [] [] = True
listachar [] [] _ = False
listachar [] _ [] = False
listachar _ [] [] = False
listachar _ _ [] = False
listachar [] _ _ = False
listachar _ [] _ = False
listachar (a:bc) (d:ef) (g:hi) | (a == d) && (d==g) = listachar bc ef hi
                               | otherwise = False
-- ##############################################################################################################################################################

3{- Faça uma funcao que some todos os elementos de uma lista de int e retorne o resultado-}

somarlista :: [Int] -> Int
somarlista [] = 0
somarlista (a:bc) = a +somarlista(bc)

-- ##############################################################################################################################################################

4{-Faça uma função que junte o primeiro elemento de uma lista com o primeiro elemento de outra lista, fazendo uma lista de tupla. 
aceitando qualquer tipo de dados-}

ziper :: [t]->[z] -> [(t,z)] {-Polimorfismo, aceita qualquer tipo de dados-}
ziper (a:as) (b:bs) = (a,b) :ziper as bs
ziper _ _ = [] {-Like otherwise-} {-"_" = qualquer coisa-}

-- ##############################################################################################################################################################

5 {-Faça uma função que reverta a ordem dos elementos de uma lista-}
reverter :: [t]->[t]
reverter [] = []
reverter (a:as) = reverter(as) ++ [a]

-- ##############################################################################################################################################################

6. id x = x -- [ nao tem tipo defido, ele deve retornar o mesmo objeto x.s

-- ##############################################################################################################################################################

7. --{Exercicio banco de dados 15/03/2018, slide 4- com recursao}

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr. Norrell"), ("Fernando","Duna"), ("Juliano","Velozes e Furiosos"), ("Cinquenta tons de cinza",)]

-- livros emprestados acima

--abaixo Funções sobre o banco de dados - consultas

livros :: BancoDados -> Pessoa -> [Livro]
livros [] pessoaProcurada = []  
livros ((pessoa1,livro1):restoDaBase) pessoaProcurada
  | pessoa1 == pessoaProcurada = livro1  : livros pls pessoaProcurada
  | otherwise = livros pls pessoaProcurada
  

emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos [] livroProcurado = []
emprestimos ((p1,l1):pls) livroProcurado | (l1 == livroProcurado) = p1 : emprestimos pls  livroProcurado
                         								 | otherwise = emprestimos pls  livroProcurado

emprestado :: BancoDados -> Livro -> Bool
emprestado [] livro_Procurado = False
emprestado ((p1,l1):pls) livro_Procurado | l1 == livro_Procurado = True
                                         | otherwise = emprestado pls livro_Procurado

-- emprestado ((p1,l1):pls) livro_procurado = (l1 == livro_procurado) || emprestado pls livro_procurado
-- emprestado bd livro_procurado = (emprestimos bd livro_procurado /= [])

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd pessoa_procurada = length (livros bd pessoa_procurada)

--Funções sobre o banco de dados - atualizações

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados 
emprestar bd pessoa livro = (pessoa,livro) : bd


devolver::  BancoDados -> Pessoa -> Livro -> BancoDados 
devolver [] pessoa livro = []
devolver ((pessoa1,livro1):pls) pessoa2 livro2 |pessoa1 == pessoa2 && livro1 == livro2 = pls
                                               |otherwise = (pessoa1,livro1): devolver pls pessoa2 livro2
                                               

{-Exercicio banco de dados 20/03/2018, slide sobre Compreensao de listas (Texto complementar sugestao: http://www.facom.ufu.br/~madriana/PF/TP3-Listas.pdf)-}

livros' :: BancoDados -> Pessoa -> [Livro]
livros' bd pessoa_procurada = [liv | (pessoa,liv) <- bd, pess == pessoa_procurada]

emprestimos':: BancoDados -> Livro ->[Pessoa]
emprestimos' bd livro_procurado = [pessoa | (pessoa,livro) <- bd, livro ==livro_procurado ] -- formando de entrada bd livro e escopo da funcao o que vem depoisem [escopo]

emprestado' :: BancoDados -> Livro -> Bool
emprestado' bd livro_procurado = [pess | (pess, liv) <- bd, liv == livro_procurado] /= [] -- diferente da lista vazia entao ta emprestado senao, significa que nao  tá emprestado

qtdEmprestimos' :: BancoDados -> Livro -> Int
qtdEmprestimos' bd livro_procurado = length [pess | (pess, liv) <- bd, liv == livro_procurado]

devolver' :: BandoDados -> Pessoa -> Livro -> BancoDados
devolver' bd pessoa_procurada livro_procurado = [(pess, liv) | (pess, liv) <- bd, (pess, liv) /= (pessoa_procurada, livro_procurado)]

--Algoritmo quick
20/3
quesorte :: [Int] -> [Int]
quesorte [] = []
quesorte (x:xs) = quesorte [y|y <- xs , y<x] ++ [x] ++ quesorte [y|y <- xs , y>=x]

--[press | (pess, livro) <- baseExemplos, livro == "Duna", head pess == 'A'] esses testes são feito na main. A virgulacao serve como  &&.
-- [i | i<- [1..5]]
-- [(i,j) | i <- [1..5], j <- ['a'..'c']] -- com dois "geradores" é como se foss eum lopp dentro do outro fixa o i e roda o j. 1a 1b 1c, 2a 2b 2c, etc.. 
-- sugestao para eliminar elementos duplicados desses dois loops.

-- Aula 22/03/2018

-- remover elementos duplicados
uniq ::[Int] -> [Int]
uniq [] = []
uniq (a:as) = a: uniq[ x | x <- as, x/=a]

--inserir o elemento na lista de maneira ordenada
ins :: Int -> [Int] -> [Int]
ins n [] = [n]
ins n (a:as) | n <= a = n(a:as)
						 | n > a = a:(ins n as)

inSort :: [Int] -> [Int]
InSort [] = []
--inSort (a:as) = ins a as -- nao funciona
inSort (a:as) = ins a (inSort as)

--Generalizacao e Poliformismo

note :: Bool -> Bool
note True = False
note False = True

funcao :: (a -> a) -> a ->a
funcao f x = f(f x) -- ta chamando a funcao qui duas vezes.

crescente ::Int -> Int
crescente 0 = 0
crescente n = n

ufc :: Int -> Int-- funcao para testar se e crescente
ufc 0 = 100
ufc n = 100 - n

isCrescent :: (Int -> Int) -> Int -> Bool-- resolucao de Pedro do basquete, o  hacker
isCrescent f 0 = True
isCrescent f n | f n >= (f (n-1)) && isCrescent f (n-1) = True
               | otherwise = False 


--resolução do professor
isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n = (f n > f(n-1)) && isCrescent f (n-1)


--Q1 L0
digitSum :: [Int] -> Int
digitSum [] = 0
digitSum [x] = 0
digitSum lista =  foldr1 (+) (juntar (circular lista) (repetidos lista))


repetidos :: [Int] -> [Int]
repetidos [] = []
repetidos [x] = []
repetidos (x:xy:xs)
                  |x==xy = [x] ++ repetidos(xy:xs)
                  |otherwise = repetidos(xy:xs)

circular:: [Int] ->[Int]
circular [] = []
circular [x] = []
circular (x:xy:xs)| x == last(xs) = [x]
                  |otherwise = []


juntar = (++)
---



existe :: [Int]->Int -> Bool
existe [] _ = False
existe (x:xs) y  | y/=x = existe xs y
                 | otherwise = True

digitos :: String -> String
digitos x = [y | y<- x , y =='0' || y=='1'|| y=='2'|| y=='3' || y=='4' || y=='5' || y=='6' || y=='7' || y=='8' || y=='9']
          

sumPairs :: [(Int,Int)]->[Int]
sumPairs [] = []
sumPairs ks = [a+b | (a,b) <- ks]

-- ##############################################################################################################################################################
--Q3 AP1
combinations :: [Int] -> [[Int]]
combinations [] = [[]]
combinations (x:xs) = (combinations xs) ++ map (x :) (combinations xs)
 -- ##############################################################################################################################################################
-- Aula dia 03/04/2018
data Estacao = Inverno | Verao | Outono | Primavera
    deriving (Show)

clima :: Estacao -> Estacao
clima Inverno = Verao
clima _ = Inverno

type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade



data Shape = Circle Float | Retangle Float Float -- Construtores

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr 

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

area :: Shape ->Float -- Função utilizando Construtores/ pode ser polimórfica
area (Circle r) = pi*(r*r)
area (Retangle b h) = h*b

--Exercícios da aula
showExpr :: Expr ->String
showExpr (Lit n) = show n 
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++")"

toList :: List t ->[t]
toList Nil = []
--toList (Cons t (List t))-- nome de tipo nao aparece na funcao, erro comum
toList (Cons x xs)= x : (toList xs)

## Questão prova, 05/04/2018
data Instr = PUSH Int 
            | POP 
            | ADD 
            | SUB 
            | DUP

type Pilha = [Int]
eval ::  Instr -> Pilha -> Pilha
eval (PUSH n) pilha = n:pilha
eval (POP) (x:xs) = xs
eval (ADD) (a:b:xc) = (a+b):xc
eval (SUB) (a:b:xc) = (a-b):xc
eval (DUP) (h:t) = h:(h:t)
--PAREI AQUI

evalProg :: [Instr] -> Pilha -> Pilha
evalProg [] pilha = pilha
evalProg [i] pilha = eval i pilha
evalProg (h:t) pilha = evalProg t (eval h pilha)


--Aula dia 12/04/2018 PLC
take1 :: Int -> [k]-> [k]
take1 0 (a:as) = []
take1 1 (a:as) = [a]
take1 _ [] = []
take1 y (a:as) =  [a] ++ take1 (y-1) (as)

drop1 :: Int->[t]->[t]
drop1 0 (a:as) = (a:as)
drop1 1 (a:as) = (as)
drop1 _ [] = []
drop1 y (a:as) = drop1(y-1) as

zip1 :: [t] -> [u] -> [(t,u)]
zip1 [] [] = []
zip1 (a:as)  (b:bs) = [(a,b)] ++ zip1 (as) (bs)

primos :: Int ->[Int] ->[Int]


separa :: Int->[Int] ->[Int] -- Filter

-- DANIEL 


nomes::[(Int,String,Float)] -> [String]
nomes lista = [b | (a,b,c) <-lista]


type Pessoa = String
type Filme = String
type BancoDados = [(Pessoa, Filme)]

qsorte :: [Int] -> [Int]
qsorte [] = []
qsorte (x:xs) = qsorte [y | y <-xs , y<x] ++ [x] ++ qsorte [k| k<-xs, k>=x]

bd :: BancoDados
bd = [("Rogerinho", "Duro de matar 2"),("Renan", "Harry Potter"),("Renan", "Stranger Fings"),("Julhinho", "Velozes e Furiosos"),("Mitter","O poderoso Chefao")]

--Funções sobre o Banco de Dados 

filmes :: BancoDados -> Pessoa -> [Filme] -- Busca o filme da pessoa
filmes banco pessoa_procurada = [filme | (nome,filme) <-banco , nome == pessoa_procurada ]

emprestimos :: BancoDados -> Filme ->[Pessoa] -- Retorna a lista de pessoas com o filme
emprestimos bd filme_procurado = [pessoa | (pessoa,filme) <- bd,filme==filme_procurado]

emprestado :: BancoDados -> Filme -> Bool -- Retorna True caso o filme esteja emprestado
emprestado bd filme_procurado | ([filme | (pessoa,filme) <-bd, filme==filme_procurado] == []) = False
                              | otherwise = True


qtdEmprestimos :: BancoDados -> Pessoa -> Int -- Retorna a quantidade de empréstimos que a pessoa tem
qtdEmprestimos banco pessoa_procurada = length [a | (a,b) <- banco, a == pessoa_procurada]

emprestar :: BancoDados -> Pessoa -> Filme -> BancoDados -- Inserir
emprestar bd [] [] = bd
emprestar  banco pessoa_inserir filme_inserir = (pessoa_inserir,filme_inserir):banco


devolver :: BancoDados -> Pessoa -> Filme -> BancoDados --Excluir
devolver [] _ _ = []
devolver banco [] [] = banco
devolver banco pessoa_excluir filme_excluir = [(pessoa,filme)| (pessoa,filme) <- banco, pessoa /= pessoa_excluir || filme /= filme_excluir ]

logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

tiposDeAcesso :: String -> (Int,Int)
tiposDeAcesso (a:as) = (funcaoN (a:as), funcaoD (a:as))


funcaoN :: String -> Int
funcaoN [] = 0
funcaoN (a:as)  | a == 'N' = 1 + funcaoN (as)
                | otherwise = funcaoN (as)

funcaoD :: String ->Int
funcaoD [] = 0
funcaoD (a:as) | a =='D' = 1+funcaoD as
               | otherwise = funcaoD (as)
               

--{- 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
 --  do menor para o maior elemento..
 --  exemplo: isSorted [1,6,8,9,9] ------> True
   --         isSorted [1,6,8,7,9] ------> False
  -- Dica: verifique se sua resposta funciona para listas de tamanho ímpar.
---}
isSorted :: Ord t => [t] -> Bool
isSorted lista  | quik lista == lista = True
                | otherwise = False 

quik :: Ord t => [t] ->[t]
quik [] = []
quik [x] = [x]
quik (x:xs) = quik[y | y<-(x:xs) , y<x] ++ [x] ++ quik[k | k<-(xs),k>=x]


{- 2) (2.5) O método de ordenação bubble-sort funciona da seguinte forma: 
   cada elemento da lista de entrada é comparado com o seguinte, 
   e se eles não estiverem em ordem (do menor para o maior) sua posição na lista resultante é trocada,
   e a comparação continua com a nova ordem.Esse processo é repetido até que a lista esteja ordenada 
   (nenhuma troca seja mais necessária).
   exemplo, passo a passo: 
       bSort [4,8,3,6,1,8] ----> compara 4 e 8, 8 e 3 (troca, pois 8 > 3), 8 e 6(troca novamente), 8 e 1 (troca novamente) e 8 e 8  
                                   ----> [4,3,6,1,8,8]
       repetindo o processo, temos  ---> [3,4,1,6,8,8] ---> [3,1,4,6,8,8]  ---> [1,3,4,6,8,8]
Implemente a função bSort.
Dica 1: use funções auxiliares, que façam parte do processo;
Dica 2: verifique que sua solução funciona para listas de tamanho ímpar.
-}
bSort :: Ord t => [t] -> [t]
bSort (x:y:xs) | isSorted (x:y:xs) = (x:y:xs)
               | otherwise = bSort (comparar (x:y:xs))

comparar :: Ord t => [t] -> [t]
comparar [] = []
comparar [x] = [x]
comparar (x:y:xs) | x>y = y: comparar (x:xs)
comparar (x:y:xs) | otherwise =  x:comparar(y:xs)
--{- 3) (2.5) explique como funciona e informe qual o resultado da execução das 
--   seguintes expressões. Caso estejam erradas explique por que.
--a) map (\x -> x + x) [3,5,7,9]   == [6,10,14,18]
--map vai aplicar a função lambda (\x ->x + x) para cada elemento da lista
--b) filter (\x -> x < 7) [5,7,9,11] == [5]
--filter vai filtrar os elementos, aplicando a funcao lambda (\x -> x <7) na lista [5,7,9,11] , resultando em [5]

--c) foldr1 (*) [-2,0,2,4] == 0
--a função foldr1 (*), vai aplicar a função multiplicação no primeiro elemento da lista e vai usar o resultado para multiplicar com o próximo elemento recursivamente
--d) foldr (+) 20 [-2,0,2,4] == 24
--foldr é semelhante ao foldr1, sendo que o foldr tem um argumento a mais, ele usa foldr (+) 20, ou seja, aplica a função (+) e o valor inicial não será o primeiro elemento da lista
-- mas sim o 20, que é um parametro passado na função foldr, então vai somar o 20 com o primeiro elemento e o resultado disso, vai somar com o proximo elemento da lista recursivamente.
--e) (map (+2) . filter (<7)) [5,7,9,11] == map (+2) [5] = [7]
--nesse caso, o programa vai executar primeiro a função filter e filtrar os elementos <7, então o resultado dessa função vai ser a entrada da função map (+2) , resultando em [7]
--}


{- 4) (2.5) Dada o tipo de dados 



t, abaixo, que reresenta uma árvore binária 
com informações (valores) em seus nós, faça uma função isSortedTree que informa se uma árvore está ordenada, ou seja, os valores em nós ou folhas na sub-àrvore à esquerda são sempre menores ou iguais ao valor do nó, e os da sub-árvore à direita sempre maiores ou iguais.
-}
-- data Tree t = Node t (Tree t) (Tree t) 
--             | Leaf t
-- testeOrdenado :: Tree Int
-- testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))
-- testeNaoOrdenado :: Tree Int
-- testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))
-- isSortedTree testeOrdenado ----> True
-- isSortedTree testeNaoOrdenado ----> False
-- isSortedTree :: Ord t => Tree t -> Bool

-- 1) (2.0) Escreva uma funcao locate, que recebe como entrada um elemento e uma lista de elementos, e retorna a localização (o índice) daquele elemento dentro da lista. 
-- A primeira posição na lista tem índice 0 (zero).
-- Caso o elemento não pertença à lista, deve ser retornado o valor (-1).
-- Exemplos: locate 'x' "abcdewxyz" ------>  6
--           locate 5   [5,98,7,32] ------>  0
--           locate True [False, False] --> -1
locate :: Eq t => t -> [t] -> Int
locate x [] = -1
locate x (y:ys) | existe x (y:ys) == False = -1
                | x/=y = 1 + locate x ys
                | otherwise = 1+ locate x ys

existe ::Eq t => t -> [t] -> Bool
existe x [] = False
existe x (y:ys) | x==y = True
                | otherwise = existe x ys







--FIM DANIEL INICIO MITTER
fsq :: Int -> Int -> Int
fsq x y = (x*y) - 1

par :: Int -> Bool
par n = (n > 0)

maxi :: [Int] -> Int
maxi [] = 0
maxi (a:as) = max a (maxi as)

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n = (f n) >= (f (n-1)) && (isCrescent f (n-1))

foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' f z [] = z
foldr' f z (a:bs) = (f a (foldr' f z (bs)))

map' :: (t -> u) -> [t] -> [u]
map' f [] = []
map' f (a:as) = f a : (map' f as)

filter' :: (t->Bool) -> [t] -> [t]
filter' f [] = []
filter' f (a:as) | f a = a:filter' f as
                 | otherwise = filter' f as

takeWhile' :: (t->Bool) -> [t] -> [t]
takeWhile' f [] = []
takeWhile' f (a:as) | f a = a:takeWhile' f as
                    | otherwise = []

--maiores :: [[Int]] -> [Int]
--maiores (a:as) = (map' maxi a) : maiores as
  
-- FIM MITTER INICIO VICTOR 
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- 
--Q2
--
data Temperature = Celsius Float | Fahrenheit Float | Kelvin Float   

showTmp :: Temperature -> String
showTmp (Celsius x)      = "Temp: " ++ show x ++ "º C"
showTmp (Fahrenheit x)   = "Temp: " ++ show x ++ "° F"
showTmp (Kelvin x)       = "Temp: " ++ show x ++ " K"

toKelvin :: Temperature -> Float
toKelvin (Celsius x)        = (x + 273)
toKelvin (Fahrenheit x)     = ((((x - 32) / 9) * 5) + 273)
toKelvin (Kelvin x)         = x

cmpTmp :: (Float -> Float -> Bool) -> Temperature -> Temperature -> Bool
cmpTmp f (Kelvin x) (Kelvin y)             = f x y
cmpTmp f (Celsius x) (Celsius y)           = f x y
cmpTmp f (Fahrenheit x) (Fahrenheit y)     = f x y 
cmpTmp f (Kelvin x) (Celsius y)            = f x (toKelvin (Celsius y))
cmpTmp f (Kelvin x) (Fahrenheit y)         = f x (toKelvin (Fahrenheit y))
cmpTmp f (Celsius x) (Fahrenheit y)        = f (toKelvin (Celsius x)) (toKelvin (Fahrenheit y))
cmpTmp f (Celsius x) (Kelvin y)            = cmpTmp f (Kelvin y) (Celsius x)
cmpTmp f (Fahrenheit x) (Kelvin y)         = cmpTmp f (Kelvin y) (Fahrenheit x) 
cmpTmp f (Fahrenheit x) (Celsius y)        = cmpTmp f (Celsius y) (Fahrenheit x)

instance Show Temperature where
        show = showTmp

instance Eq Temperature where
        (==) = (cmpTmp (==))

instance Ord Temperature where 
        (<=)    = (cmpTmp (<=))
        (>) x y = (cmpTmp (>)) y x

minMax :: [Temperature] -> (Temperature, Temperature)
minMax [] = error "There\'s no min or max to a Empty list."
minMax l = (minimum l, maximum l)

--
-- Q3
-- 

data LQueue t = LQ [t] deriving (Show)
data RQueue t = Empty | RQ t (RQueue t) deriving (Show)

class OprQueue q where
    enqueue :: q t -> t -> q t
    dequeue :: q t -> q t
    peek    :: q t -> t
    isEmpty :: q t -> Bool

instance OprQueue LQueue where
    isEmpty (LQ [])     = True
    isEmpty (LQ (a:as)) = False
    peek    (LQ [])     = error "Empty Queue!"
    peek    (LQ (a:as)) = a
    dequeue (LQ [])     = error "Empty Queue!"
    dequeue (LQ (a:as)) = (LQ as)    
    enqueue (LQ []) x   = (LQ [x])
    enqueue (LQ d)  x   = (LQ (d ++ [x]))

instance OprQueue RQueue where
    isEmpty (Empty)  = True
    isEmpty (RQ x y) = False
    peek    (Empty)  = error "Empty Queue!"
    peek    (RQ x z) = x   
    dequeue (Empty)             = error "Empty Queue!"
    dequeue (RQ x Empty)        = (Empty)
    dequeue (RQ x (RQ y z))     = (RQ y z)         
    enqueue (Empty) x           = (RQ x Empty)
    enqueue (RQ a b) x          = (RQ a (enqueue b x))
    
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- FIM VICTOR 


--PROVA 2016.2


logSetembro :: String
logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"


--1)

tiposDeAcesso :: String -> (Int, Int)
tiposDeAcesso str = (count "Normal" str, count "Denied" str)
where count search [] = 0
      count search str | search == take 6 str = 1 + count search (drop 6 str) 
                       | otherwise = count search (tail str)
      
--poderia usar o spitAt, profe vai continuar na aula q vem
--FIM do Julis <3

--2)


--- lista pessoal de cc (By MJuliano <3)

--1)  [1,2,2] = f[2]  ou [1,2,2,2,3,3] = f[2,2,3] ou [1,2] = f []
f :: [Int] -> [Int]
f [] = [] 
f [x] = []
f (x:y:xs)
	| x == y = [x] ++ f (y:xs)
  |otherwise = f (y:xs) 
  
--1)a- faca o mesmo programa agora com compreensao de lista
f l = [l!!x | x <- [1..((length l) - 1)], l!!x == l!!(x-1)] -- !! serve pra pegar o index do array!!!!!

--2)verifaica que todo elemento q tam entre 10 e 100 é par fazendo com map filter folder
--posso ver se ta entre dez e 100, se tiver verifico se é par? Mas tem q ser com map filter e foldr
--pro map damos uma funcao e uma lista ai pegamos caa delemento e aplicado aessa funcao colocar em outra lista.
-- usamos filter pra pegar somente os elem de 10 a 100
--podemos encaixar o map aqui pra fazer o mod 2 == 0 pra ver se é par
--agora usamos o foldr vindo da esq da dir aplicando determinada funcao, aplicamos a funcao foldr com o and se algum for false pra retorna falso
helper :: Int -> Bool
helper n = n >= 10 && n < = 100

g :: [Int] -> [Bool]
g l = foldr (&&) True (map (\x -> x `mod`2 == 0) (filter helper l)) --for true eu pego pra mim, fultro. esse true é o primeiro element q tem ser aplicaod na lista,
--é obg no foldr tem o foldl1 que nao precesaria do true, coloca pq tru e tru é tre
--essa \x e a notacao lambda que vai redefier todos os paramentos da sua funcao. o x é o paramentro agora vou pra notacao da 
--,mihafuncao qual vai ser minha funcao x mod 2 -- 0 se o elmento é par ai vai dar true ou false. ai aplico folder r essa lista com uma funcao and

--3) 
data Tree = null | Node Int Tree Tree
--data Tree a = null | Node a (Tree a )  (Tree a) -- portanto a arove pode ser q qq tipo agora, posso ainda mais fazer
--data Tree a = null | Node a (Tree a )  (Tree a) deriving (Show) 
--https://gist.github.com/Kedrigern/1239141 implementacao geral de arvore ver dps
--ver diferenca entre data e type, o type é tipo typedef, uma macro de c c++ dizer que é tipo de outro tipo type Nome = String
-- e o data é completamente difernte essa minha data é mais parecido com uma struct de c++  quando crio uma data tree eu coloco os possiveis construtores que vem definidos ai, a barra | ser pra ver qual vai ser o construtor.
inorder :: Tree -> String
inorder (Null) = " "--note abaixo que show é uma instancia de int entao pode imprimir o n
inorder (Node n l r) = (inorder l) ++ " " ++ (show n) ++ " "  ++ (inorder r)

instance Eq Tree where -- definindo instancia de eq em tree
null == null = True
(Node x l1 r1) == (Node y l2 r2) = x == y && l1 == l2 && r1 == r2 --olha q bizarro vc chama a instancia recursivamente ao definir ela mesma...
_ == _ = False


--4) Defina tipo algebrico de lampada
type NomeFrab = String
type Potencia = Float
data Lampada = Compacta NomeFab Potencia | Incandescente NomeFrab Potencia --ve que :t Lampada nao existe, agora teste :t Compacta
--lampada nao é um construtor é como se fosse uma classe pai abastrata ou interface de OO

instance Show Lampada where
 show (Compacta nomeFab pot) = "Compacta" ++ nomeFrab ++ " " ++ (show pot) --pronto casamos padrao e show pot temos qusar assim, pois ele é float.
 show (Incadescente nomeFab pot) = "Compacta" ++ nomeFrab ++ " " ++ (show pot) --pronto casamos padrao e show pot temos qusar assim, pois ele é float.
--teste no prelude:  (show lamp1) ++ "oi", dps faça show lamp

--5)
-- observacao testa no prelude: let funcplus2 = (2+) note que fixamos o primeiro parametro + alguma coisa entao funcplus2 4 = 6


functions :: [(a->a->a)] -> [a] -> [(a->a)] --lista de funcao de 2 param agora tem um lista e como resutlado uma lista funcoes de (a->a), mandou fixar o 1 param vai ter somente o resto a>a foda-se ai eu vou casar esse 1 param com a outra entrada [a]
functions [] [] = []
functions (x:xs) (y:ys) = [x y] ++ functions xs ys

--functions :: [(a->a->a)] -> [a] -> [(a->a)]
--note a questao é essa se vc comentar o tipo como feita ai acima e testar
-- let myfuct = [(+),(-),(*)] [2,6,9] se vc testar no prelude nao posso imprimir a funcao pq ela nao tem show definido se vc rodar fuctions myfuct [1,2,3] vai resultar [2,3,27]


apply_func :: [(a->a)] -> [a] -> [a]
apply_func [] [] = []
apply_func (x:xs) (y:ys) = [x y] ++ apply_func xs ys

--Fim Lista de CC By Julindo 
(a->a->a) = (a->a)  

-- FIM JULIANO inicio VICTOR && MITTER
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥

type Dia = String
type Hora = String
type Usuario = String
data LogEntry = Permitido Dia Hora Usuario | Negado Dia Hora Usuario
     deriving (Show)

logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

-- Função dada na questão
strToInt :: String -> Int
strToInt str = read str

--
--
--      Q4
-- 
--

convertLog :: String -> [LogEntry]
convertLog [] = []
convertLog str = toLogEntry (take 34 str) : convertLog (drop 35 str)


toLogEntry :: String -> LogEntry
toLogEntry "" = error "Cannot convert"
toLogEntry str | ((slice str 20 6)) == "Normal" = (Permitido (slice str 8 2) (slice str 11 8) (slice str 27 6))
               | otherwise = (Negado (slice str 8 2) (slice str 11 8) (slice str 27 6))

slice :: String -> Int -> Int -> String
slice [] _ _ = []
slice str n m = take m (drop n str) 


--
--      Q1
--
(+++) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+++) (x,y) (a,b) = ((x+a), (y+b))

countAll :: [LogEntry] -> (Int,Int)
countAll [] = (0,0)
countAll ((Permitido x y z):as) = (1,0) +++ (countAll as)
countAll ((Negado x y z):as) = (0,1) +++ (countAll as)

-- Just for be THE SAME OF QUESTION WANT
tiposDeAcesso :: String -> (Int, Int)
tiposDeAcesso str = countAll (convertLog str)

--
-- Functions below are useful to Q2 and Q3
--

-- Get value of prop in LogEntry data
sProp :: String -> LogEntry -> String
sProp [] _ = error "You must passed a valid prop"
sProp "Dia" (Permitido x y z)        = x
sProp "Hora" (Permitido x y z)       = y
sProp "Usuario" (Permitido x y z)    = z
sProp "Dia" (Negado x y z)           = x
sProp "Hora" (Negado x y z)          = y
sProp "Usuario" (Negado x y z)       = z

countDays :: String -> String -> [LogEntry] -> [(Int,Int)]
countDays _ _ [] = []
countDays p beforeSearch (a:as) | beforeSearch == (sProp p a)  = (countDays p (sProp p a) as) 
                                | otherwise                    = ( (strToInt (sProp p a)), (countDay p (sProp p a) (a:as))) : (countDays p (sProp p a) as) 

countDay :: String -> String -> [LogEntry] -> Int
countDay _ _ [] = 0
countDay p x (a:as) | x == (sProp p a) = 1 + (countDay p x as)
                    | otherwise       = (countDay p x as)

--
--      Q2
--

-- Just for be THE SAME OF QUESTION WANT
acessoPorDia :: String -> [(Int,Int)]
acessoPorDia str = (countDays "Dia" "0" (convertLog str))

--
--     Q3
--
acessosPorUsuario :: String -> [(Int, Int)]
acessosPorUsuario str = (countDays "Usuario" "0" (convertLog str))



--
--
--
-- Q2 or Q3 on HARD way you need to set X, Y or Z that you want to Compare!
--
--  In hard way you need repeat functions below for each "query", Usuario or Day
--
countDaysWithoutSProp :: String -> [LogEntry] -> [(Int,Int)]
countDaysWithoutSProp _ [] = []
countDaysWithoutSProp beforeSearch ((Permitido x y z):as) | beforeSearch == x    = (countDaysWithoutSProp x as) 
                                              | otherwise         = ((strToInt x), (countDayWithoutSp x ((Permitido x y z):as))) : (countDaysWithoutSProp x as) 

countDaysWithoutSProp beforeSearch ((Negado x y z):as)    | beforeSearch == x    = (countDaysWithoutSProp x as) 
                                              | otherwise         = ((strToInt x), (countDayWithoutSp x ((Negado x y z):as))) : (countDaysWithoutSProp x as) 

countDayWithoutSp :: String -> [LogEntry] -> Int
countDayWithoutSp x [] = 0
countDayWithoutSp x ((Permitido a y z):as)   | x == a      = 1 + (countDayWithoutSp x as)
                                    | otherwise   = (countDayWithoutSp x as)
countDayWithoutSp x ((Negado a y z):as)      | x == a      = 1 + (countDayWithoutSp x as)
                                    | otherwise   = (countDayWithoutSp x as)

-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- FIM VICTOR && MITTER inicio jujubs e dudubs

--
--     Q1 2017.1
--
--solucao 1
isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (a:b:as) | a <= b = isSorted(b:as)
                  | otherwise = False
--solucao 2
qSort :: Ord t => [t] -> [t]
qSort [] = []
qSort (a:as) = qSort [i | i <- as , i < a] ++ [a] ++ qSort [i | i <- as , i >= a]

func :: Ord t => [t] -> Bool
func [] = True
func as = as == (qSort(as))
--
--     Q2 2017.1
--              
bSort :: Ord t => [t] -> [t]
bSort list | IsSorted list = list
  	       | otherwise = bSort (faztrocas list) --ver porque pode fazer isso, deve ser por conta da classe ord
            
faztrocas :: Ord t => [t] -> [t]
faztrocas [] = []
faztrocas [x] = [x]
faztrocas (a:b:rabo) | (a > b) = b : faztrocas (a:rabo) 
                     | otherwise = a : faztrocas (b:rabo)
                     
                     
--
--     Q3 2017.1 escrita fazer
--        


--
--     Q4 2017.1
--        
 data Tree t = Node t (Tree t) (Tree t) 
           | Leaf t
testeOrdenado :: Tree Int
testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))
testeNaoOrdenado :: Tree Int
--isSortedTree testeOrdenado ----> True
--isSortedTree testeNaoOrdenado ----> false
isSortedTree :: Ord t => Tree t -> Bool
--o nome do tipo nunca vai aparecer em seu code, somente o nome do construtor... como foi ressaltado na questao da lampada de CC acima.  tipo isSortedTree(Node value (Tree t1) (Tree t2)), o correto seria (Node value t1 t2)
isSortedTree (Leaf value) = True
-- *** isSortedTree (Node value t1 t2) = isSortedTree t1 && isSortedTree t2 && compare1 (>=) value t1 && compare1 (<=) value t2)  
isSortedTree (Node value t1 t2) = isSortedTree t1 && isSortedTree t2 && isGreaterOrEqualThan value t1 && isLeastThan value t2)  

isGreaterOrEqualThan  :: Ord t => t -> Tree t -> Bool
isGreaterOrEqualThan (value v1) = (value == v1)
isGreaterOrEqualThan (value v1 t1 t2) = value >= v1 && isGreaterOrEqualThan value t1 && isGreaterOrEqualThan value t2 -- comprar raiz com subarvores

isLeastThan  :: Ord t => t -> Tree t -> Bool
isLeastThan (value v1) = (value == v1)
isLeastThan (value v1 t1 t2) = value <= v1 && isLeastThan value t1 && isLeastThan value t2 -- comprar raiz com subarvores

--  *** mas podemos abtrair e fazer uma funcao so ao inves de duas IGET e ILT
compare1  :: Ord t => (t -> t -> Bool) -> t -> Tree t -> Bool
compare1 cmp (value v1) = (value == v1)
compare1 cmp (value v1 t1 t2) = value `cmp` v1 && compare1 cmp value t1 && compare1 cmp value t2 -- comprar raiz com subarvores

--poderia fazer tbm com flatten  (parece q é predefinida)
flaten' :: Tree t -> [t]
flaten' (Leaf value) = [value]
flaten' (Node value t1 t2) = flaten' t1 ++ [value] ++ flaten' t2

-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- FIM JUJUBS && DUDUBS
-- INICIO MURIMURI

--
-- Prova 2015-1
--

type Resultado = [Int]
type Jogo = [Int]
type Jogos = [[Int]]

premiados :: Resultado -> Jogos -> Int
premiados _ [] = 0
premiados r (j:js) | (r == j) = 1 + premiados r js
                   | otherwise = premiados r js

presente :: Int -> Resultado -> Bool
presente _ [] = False
presente x (a:ax) | (x == a) = True
                  | otherwise = presente x ax 

acertosUnicos :: Resultado -> Jogo -> Int
acertosUnicos _ [] = 0
acertosUnicos r (a:ax) | presente a r = 1 + acertosUnicos r ax
                       | otherwise = acertosUnicos r ax

acertos :: Resultado -> Jogos -> [Int]
acertos _ [] = []
acertos r (j:js) = [acertosUnicos r j] ++ acertos r js

count :: [Int] -> Int -> Int
count j i = length [x | x <- j, x == i]

numPremios :: Resultado -> Jogos -> (Int, Int, Int)
numPremios r j = (count (acertos r j) 4, count (acertos r j) 5, count (acertos r j) 6)

data Instrucao = PUSH Int 
                | POP 
                | ADD 
                | SUB
                | DUP
                deriving (Show)

type Pilha = [Int]

eval :: Instrucao -> Pilha -> Pilha
eval ADD (a:b:cs) = (a+b):cs
eval DUP (a:cs) = (a:a:cs)
eval SUB (a:b:cs) = (a-b):cs
eval (PUSH i) cs = i:cs
eval POP (a:cs) = cs

evalProgAux :: [Instrucao] -> Pilha -> Pilha
evalProgAux [] pi = pi
evalProgAux (a:as) pi = evalProgAux as (eval a pi) 

evalProg :: [Instrucao] -> Pilha
evalProg cs = evalProgAux cs []

data Expr = Literal Int
          | Soma Expr Expr
          | Subtrai Expr Expr
          | Dobra Expr

translate :: Expr -> [Instrucao]
translate (Literal i) = [PUSH i]
translate (Soma e1 e2) = translate e2 ++ translate e1 ++ [ADD]
translate (Subtrai e1 e2) = translate e2 ++ translate e1 ++ [SUB]
translate (Dobra e1) = translate e1 ++ [DUP]

-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- FIM MURIMURI inicio JUJUBAA


maior :: [Int] -> Int
maior [x] = x 
maior (x:xs) | (x > maior xs) = x
             | otherwise = maior xs

todos_pares :: [Int] -> Bool
todos_pares [] = True --tem q considerar que o vazio é true
todos_pares (x:xs) | (mod x 2 == 0) = (True && todos_pares xs)
                   | otherwise = False

inv_aux:: [t] -> [t] -> [t] --a segunda lista é a que vai acumular
inv_aux [] l_inv = l_inv
inv_aux (x:xs) l_inv = inv_aux xs l_inv ++ [x]--elementos sempre add ao final da lista, note que estamos colocando x como [x] concatentando 	


inv_lista :: [t] -> [t]
inv_lista [] = []
inv_lista l = inv_aux l [] 


comp_lista :: [Int] -> [Int] -> Bool
comp_lista [] []  = True
comp_lista [] _   = False
comp_lista _ []   = False
comp_lista (x:xs) (y:ys) | (length (x:xs) /= length (y:ys)) = False
                         | otherwise = compareListas (x:xs) (y:ys)
                                 where compareListas [x] [y] = (x == y)
                                       compareListas (x:xs) (y:ys) = if (x == y)
                                                                      then compareListas xs ys
                                                                    else False

pertence:: [Int] -> Int -> Bool 
pertence [] _ = False
pertence (x:xs) num | (num == x) = True
                    | otherwise = pertence xs num


ppertence:: (Eq t) => [t] -> t -> Bool 
ppertence [] _ = False
ppertence (x:xs) num | (num == x) = True
                     | otherwise = ppertence xs num


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome x = compareLists x (reverse x)
     where compareLists [x] [y]       = x == y
           compareLists (x:xs) (y:ys) = if x == y
                                         then compareLists xs ys
                                        else False

iisPalindrome :: (Eq a) => [a] -> Bool
iisPalindrome [] = False
iisPalindrome x = compareLists2 x (reverse x)
   

compareLists2 :: (Eq a) => [a] -> [a] -> Bool
compareLists2 [] [] = False
compareLists2 [] _  = False
compareLists2 _ []  = False
compareLists2 [x] [y] = (x == y)
compareLists2 (x:xs) (y:ys) | x == y = compareLists2 xs ys
                            | otherwise = False

size_list :: (Eq a) => [a] -> Int
size_list [] = 0
size_list (x:xs) = 1 + size_list xs


type Nome = String
type Idade = Int
type Lingua = String
type Pessoa = (Nome, Idade, Lingua)

pessoa :: Pessoa
pessoa = ("ju", 21, "arabe")

nomes::(String,String,String)
nomes = ("Juliano", "Cynthia", "Carine")

selec1:: Pessoa -> Nome
selec1 (x,_,_) = x -- pra printar trecos das tuplas.y 
selec3 (_,_,z) = z
 -------------------

tabela :: Int -> String
tabela n = cabeçalho ++ imprimeAlunos n ++ imprimeMedia n

cabeçalho :: String
cabeçalho = "Aluno Nota\n"

imprimeAlunos :: Int -> String
imprimeAlunos 1 = imprimeAluno 1
imprimeAlunos n = imprimeAlunos (n-1) ++ imprimeAluno n


imprimeAluno :: Int -> String
imprimeAluno n = show n ++ "  " ++ show (aluno n) ++ "\n"

imprimeMedia :: Int -> String
imprimeMedia n = "\n" ++ "Média da Turma: "++ show (media n)

media :: Int -> Float
media n = (soma n) / (fromIntegral n) --talvez isso nao funciona o fromIntegral, dai usar o round

soma :: Int -> Float
soma 1 = aluno 
soma n = aluno n + soma (n-1)

aluno :: Int -> Float
aluno 1 = 7.5
aluno 2 = 10.0
aluno 3 = 9.0
aluno 4 = 6.3
 -------------------

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (x:xs) = 2*x : dobraLista xs

----------------------
ordenacao :: [Int] -> [Int]
ordenacao [] = []
ordenacao (a:x) = insere a (ordenacao x)

insere :: Int -> [Int] -> [Int]
insere ele [] = [ele]
insere ele (x:xs) | (ele <= x) = ele:(x:xs)
                  | otherwise = x : insere ele xs --note que aqui vou procurando a possicao  

-----------------

fazQaudro::[Int] -> [Int]
fazQaudro [] = [] -- ** mesmo que 2^2 == 2**2, mas cuidado com o uso do **
fazQaudro listRecebida = [umElmDalista^2 |umElmDalista <- listRecebida]

fazPot ::[Int] -> [Int]
fazPot [] = [] -- ** mesmo que 2^2 == 2**2, mas cuidado com o uso do **
fazPot listRecebida = [umElmDalista^umElmDalista |umElmDalista <- listRecebida]

baseDeDados :: [(Int, String, Float)]
baseDeDados = [ (1, "André", 10.0), (2, "Carlos", 6.8), (3,"Maurício", 7.0)]

nomes :: [(Int, String, Float)] -> [String] -- ou mas facil seria nomes list = [b | (a,b,c) <-list]
nomes [] = []
nomes lista = [pegaNome a | a <-  lista]  
    where 
    pegaNome (a,b,c) = b


fazQaudro::[Int] -> [Int]
fazQaudro [] = [] -- ** mesmo que 2^2 == 2**2, mas cuidado com o uso do **
fazQaudro listRecebida = [umElmDalista^2 |umElmDalista <- listRecebida]


fazPot ::[Int] -> [Int]
fazPot [] = [] -- ** mesmo que 2^2 == 2**2, mas cuidado com o uso do **
fazPot listRecebida = [umElmDalista^umElmDalista |umElmDalista <- listRecebida]


somaPares::[(Int, Int)] -> [Int]
somaPares [] = []
somaPares lista = [ a+b | (a,b) <- lista ]


fazPares:: [t] -> [u] -> [(t,u)]
fazPares [] _ = []
fazPares _ [] = []
fazPares listaT listaU = [(a,b) | a <- listaT, b <- listaU]
--o primeiro valor da primeira lista é gerado e mantido enquanto se avalia os valores da lista seguint
--fazPares [1,2] [3,4] = [(1,3),(1,4),(3,1),(3,2)]


--Por exemplo, utilizando list Comprehensions e um predicado, pode-se criar um
--filtro para strings
remove:: Char -> [Char] -> [Char]
remove caracter str = [c | c <- str, c /= caracter]

--Haskell > remove ‘ ‘ “Este exemplo remove os espaços em branco!”
--“Esteexemploremoveosespaçosembranco!”


-----------
par :: Int -> Bool --para ser usado como condicional
par x = (mod x 2 == 0)

condicional:: [Int] -> [Int]
condicional [] = []
condicional lista = [x | x <- lista, par x, x > 4] -- essas sequencias so vao concat o que for true em tudo...

---------------
n = a `div` length xs
  where
  a = 10
  xs = [1,2,3,4,5]

-------------
--Quando há duas ou mais definições locais, elas podem ser escritas em diferentes estilos.
--Na notação básica as definições são separadas por ponto-e-vírgula (;) e escritas entre chaves ({ e }). Por
--exemplo:
f x y = (a+1)*(b+2)
where { a = (x+y)/2; b = (x+y)/3 }
--Algumas vezes as chaves podem ser omitidas:
f x y = (a+1)*(b+2)
where a = (x+y)/2; b = (x+y)/3
--Às vezes até os ponto-e-vírgulas podem ser omitidos:
f x y = (a+1)*(b+2)
where a = (x+y)/2
      b = (x+y)/3

--Com where as definições são colocadas no final, e com let elas são colocadas no início.
--• let é uma expressão e pode ser usada em qualquer lugar onde se espera uma expressão.
--• Já where não é uma expressão, podendo ser usada apenas para fazer definições locais em uma definição de
--função.

-----------ordenacao verbosa
lista :: [Int]
lista = [5,2,10,1,9]

get_menor :: [Int] -> Int
get_menor [x] = x
get_menor (x:xs) | (x < get_menor xs) = x
				 | otherwise = get_menor xs


remove_menor :: [Int] -> [Int]
remove_menor [] = []
remove_menor (x:xs) | (x == (get_menor (x:xs))) = xs
					| otherwise = (x:remove_menor xs)


aux_ordena :: [Int] -> [Int] -> [Int]
aux_ordena lista_ordenada [] = lista_ordenada
aux_ordena lista_ordenada (x:xs) = aux_ordena (lista_ordenada++[get_menor (x:xs)]) (remove_menor (x:xs))


ordena :: [Int] -> [Int]
ordena [] = []
ordena lista = aux_ordena [] lista
------------------
show e putStr para iimprimir coisas na net em formato de string
read converte de string para numero (nao lembro se é float ou int)




-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- FIM JUJUBA
