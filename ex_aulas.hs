{-and:: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

and xs = foldn (&&) True --aplicar a lista. faz o mesmo que o and


sum [] = 0
sum (x:xs) = x + sum xs
sum xs = foldr (+) 0 

product [] = 1
product (x:xs) = x * product xs
product xs = foldr (*) 1 -}


--AULAS 3 E 4

--obter pares consecutivos de elementos de uma lista
pares:: [a] -> [(a,a)]
pares xs = zip xs (tail xs)

--verifica se  alista está por ordem crescente
crescente:: Ord a => [a] -> Bool
crescente l = and [x <= y | (x,y) <- pares l]

--procura um valor numa lista e obtêm todos os seus índices
indices:: Eq a => a -> [a] -> [Int]
indices n l = [i | (i,x) <- zip [0..(length l - 1)] l, n == x]

formula:: Float -> Float -> Float -> [Float]
formula a b c | delta > 0 = [(-b + sqrt delta) / (2 * a), (-b - sqrt delta) / (2 * a)]
              | delta == 0 = [-b / (2 * a)]
              | otherwise = []
              where delta = b^2 - 2 * a * c

--dá  o número anterior
anterior:: Int -> Int
anterior n | n >= 1 = n - 1

null1:: [a] -> Bool
null1 xs = case xs of
                [] -> True
                _ -> False


--AULAS 5 E 6
--Funções de ordem superior
twice:: (a -> a) -> a -> a
twice f x = f (f x)

sum1 xs = foldr (+) 0 xs


--AULAS 7 E 8
cycle1:: [a] -> [a]
cycle1 [] = error "empty list"
cycle1 xs = xs'
         where xs' = xs ++ xs'


--AULAS 9 E 10
type  Pos = (Int,Int)
data Dir = Esq | Dir | Cima | Baixo
mover:: Dir -> Pos -> Pos
mover Esq (x,y) = (x-1,y)
mover Dir (x,y) = (x+1,y)
mover Cima (x,y) = (x,y+1)
mover Baixo (x,y) = (x,y-1)

data Figura = Circ Float
            | Rect Float Float

quadrado :: Float -> Figura
quadrado h = Rect h h

area :: Figura -> Float
area (Circ r) = pi*r^2
area (Rect w h) = w*h


--ÁRVORES SINTÁTICAS
data Expr = Val Int
          | Soma Expr Expr
          | Mult Expr Expr

arv2 = Soma (Val 1) (Mult (Val 2) (Val 3))

--contar número de folhas
tamanho:: Expr -> Int
tamanho (Val n) = 1
tamanho (Soma e1 e2) = tamanho e1 + tamanho e2
tamanho (Mult e1 e2) = tamanho e1 +  tamanho e2

-- calcular o valor
valor :: Expr -> Int
valor (Val n) = n
valor (Soma e1 e2) = valor e1 + valor e2
valor (Mult e1 e2) = valor e1 * valor e2


--ÁRVORES BINÁRIAS
data Arv0 = Folha Int
          | No0 Arv0 Int Arv0

arv3 = No0 (No0 (Folha 1) 3 (Folha 4)) 5 (No0 (Folha 6) 7 (Folha 9))

--procurar um valor na árvore
ocorre:: Int -> Arv0 -> Bool
ocorre n (Folha m) = n == m
ocorre n (No0 esq m dir) = (n == m || ocorre n esq || ocorre n dir)


--AULAS 13 E 14
data Arv a = Vazia
           | No a (Arv a) (Arv a)

arv1:: Arv Int
arv1 = No 15 (No 10 (No 5 Vazia Vazia) Vazia) (No 20 Vazia Vazia)

--listar todos os valores
listar:: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

--ver se a`árvore está ordenada
ordenada:: Ord a => Arv a -> Bool
ordenada arv = crescente (listar arv)
             where crescente xs = and (zipWith (<=) xs (tail xs))

--ver se um valor pertence à árvore
pertence:: Ord a => a -> Arv a -> Bool
pertence x Vazia = False
pertence x (No y esq dir) | x == y = True
                          | x < y = pertence x esq
                          | x > y = pertence x dir

--inserir um valor
inserir:: Ord a => a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir) | x == y = No y esq dir
                         | x < y = No y (inserir x esq) dir --inserir à esquerda
                         | x > y = No y esq (inserir x dir) -- inserir à direita

--obter o menor valor
mais_esq:: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

--obter o maior valor
mais_dir:: Arv a -> a
mais_dir (No x _ Vazia) = x
mais_dir (No _ _ dir) = mais_dir dir

--remover
remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (No y Vazia dir) | x==y = dir
remover x (No y esq Vazia) | x==y = esq
remover x (No y esq dir) | x<y = No y (remover x esq) dir
                         | x>y = No y esq (remover x dir)
                         | x==y = let z = mais_esq dir
                                  in No z esq (remover z dir)


--ÁRVORES EQUILIBRADAS
arv4:: Arv Int
arv4 = No 3 (No 2 (No 1 Vazia Vazia) Vazia) (No 5 (No 4 Vazia Vazia) (No 6 Vazia Vazia))

arv5:: Arv Int
arv5 = No 5 (No 4 (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)) Vazia) (No 6 Vazia Vazia)

altura:: Arv a -> Int
altura Vazia = 0
altura (No _ esq dir) = 1 + max (altura esq) (altura dir)

equilibrada:: Arv a -> Bool
equilibrada Vazia = True
equilibrada (No _ esq dir) = abs (altura esq - altura dir) <= 1 && equilibrada esq && equilibrada dir

