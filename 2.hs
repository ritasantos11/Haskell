--43
insert:: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n < x = (n:x:xs)
                | otherwise = x:insert n xs

--48
listc:: (a -> b) -> (a -> Bool) -> [a] -> [b]
--          f           p
--(a -> b) é da função map
--(a -> Bool) é da função filter
listc f p xs = [f x | x <- xs, p x]
--listc (*2) (<7) [1,7,4,8]
--multiplica por 2 os números da lista menores que 7
--ou seja, primeiro executa o (<7) e depois o (*2)

--usando map e filter:
listc1 f p l = map f (filter p l)

--49 a)
--concat1 [] b = b
--concat1 (x:xs) = x:concat1 xs
-- ++ recebe duas listas

--usando foldr
concat:: [a] -> [a] -> [a]
concat xs ys = foldr (:) ys xs
--(:) mete o novo elemento à cabeça da lista
--por isso é q é foldr (:) l2 l1
--(:) [4,5,6] [1,2,3]
--mete o elemento mais à direita da segunda lista (foldr) à cabeça da primeira lista (:)

--usando foldl
concat2:: [a] -> [a] -> [a]
concat2 xs ys = foldl (\x y -> (y:x)) ys (reverse xs)

--49 b)
concat3:: [[a]] -> [a]
concat3 xs = foldr (++) [] xs

--49 c)
reverse1:: [a] -> [a]
reverse1 = foldr (\x y -> y ++ [x]) []
--para concatenar, corre a lista toda para acrescentar no fim o novo elemento
--ou
--reverse1 = foldr f (x y) []
--         where f (x y) = y ++ [x]
--ou
--reverse1 l1 = foldr (\x y -> y ++ [x]) [] l1

--49 d)
reverse2:: [a] -> [a]
reverse2 = foldl (\x y -> [y] ++ x) []
--acrescenta logo a cabeçao o novo elemento

--49 e)
elem1:: Eq a => a -> [a] -> Bool
elem1 n l = any (==n) l
--o any recebe uma função e uma lista: (função) lista
--(n==) é a funçao que o any vai receber neste caso.

--50
dec2int:: [Int] -> Int
dec2int l = foldl (\x y -> 10 * x + y) 0 l

--51
zipWith1:: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f [] _ = []
zipWith1 f _ []  = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

--52
isort:: Ord a => [a] -> [a]
isort l = foldr insert [] l

insert' :: Ord a => a -> [a] -> [a]
insert' b [] = [b]
insert' b (x:xs) | b>x = x : insert' b xs  
                 | otherwise = b:x:xs

--53
shift:: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

--58
fibonacci1:: [Int] --não recebe nada, só retorna uma lista infinita
fibonacci1 = 0:1:[x + y | (x,y) <- zip fibonacci1 (tail fibonacci1)]
--ou
--fibonacci1 = 0:1:zipWith (+) (tail fibonacci1) fibonacci1
--fibonacci = []0,1,1,2,3,5,8,13,21..]
-- 0,1, 1+0, 1+1, 2+1, 3+2, 5+3, 8+5, 13+8, ...
--tail fibonacci1 = [1,1,2,3,5,8,13..]
--fibonacci = [0,1,1,2,3,5,8..]

factorial1:: [Integer]
factorial1 = 1:[x*y |  (x,y) <- zip factorial1 [1..]]

--59
merge:: [Int] -> [Int] -> [Int]
merge l [] = []
merge [] l = []
merge (x:xs) (y:ys) | x < y = x:merge xs (y:ys)
                    | x == y = merge xs ys
                    | x > y = y:merge (x:xs) ys

--60
somas:: [Int] -> [Int]
somas xs = 0:[x+y | (x,y) <- zip (somas xs) (tail xs)]

--64
elefantes:: Int -> IO()
--nao pode pôr número menor que 3. O texto começa em 2 elefantes.
elefantes n | n < 3 =  putStrLn "Número inválido. Introduza outro"
            | otherwise = sequence_[putStrLn (frase i ) | i <- [2..n-1]]
                        where frase i = "Se " ++ show i ++ " elefantes incomodam muita gente, " ++ "\n" ++ show (i + 1) ++ " elefantes incomodam muito mais!"

--65
my_wc :: FilePath -> IO ()
my_wc file = do
         text <- readFile file --dá-mos um ficheiro como string
         putStrLn (show(length(lines text)))
         putStrLn (show(length(words text)))
         putStrLn (show(length(text)))

--66
program ::IO ()
program = do  --nao tem nenhum argumento
          line <- getLine --pede uma linha
          if (null line) then do return ()
          else do putStrLn (unwords(reverse (words line))) --unwords passa lista para frase
          program --chama outra vez o programa


--Árvores
data Arv a = Vazia | No a (Arv a) (Arv a)
           deriving (Show, Eq)
--Arv a : Tipo
--Vazia e No : COnstrutores de Tipo
--Listas tem 2 construtores: Vazia e os :
--f:: Arv Int -> Int
--f Vazia = ...
--f (No x esq dir) = ...   Como nas listas

arv1:: Arv Int
arv1 = No 15 (No 10 (No 5 Vazia Vazia) Vazia) (No 20 Vazia Vazia)

--70
sumArv:: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x esq dir) = x + sumArv esq + sumArv dir

--71
listar:: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

ldecrescente:: Arv a -> [a]
ldecrescente Vazia = []
ldecrescente (No x esq dir) = listar dir ++ [x] ++ listar esq

--72
nivel:: Int -> Arv a -> [a]
nivel n Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = nivel (n-1) esq ++ nivel (n-1) dir

--74
mapArv:: (a -> b) -> Arv a -> Arv b
mapArv f Vazia = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)

--75 a)
mais_dir:: Arv a -> a
mais_dir (No x _ Vazia) = x
mais_dir (No _ _ dir) = mais_dir dir

--75 b)
remover1:: Ord a => a -> Arv a -> Arv a
remover1 x Vazia = Vazia
remover1 x (No y Vazia dir) | x == y = dir
remover1 x (No y esq Vazia) | x == y = esq
remover1 x (No y esq dir) | x < y = No y (remover1 x esq) dir
                          | x > y = No y esq (remover1 x dir)
                          | x == y = let z = mais_dir esq
                                   in No z esq (remover1 z dir)

--77 a)
data Shape = Circle Float
           | Rectangle Float Float
           deriving (Show,Eq)
--Circle e Rectangle: Construtores de tipo
--Shape e Float: Tipos

--77 b)
perimetro:: Shape -> Float
perimetro (Circle r) = 2*pi*r
perimetro (Rectangle a b) = 2*a + 2*b

-- ... x = if A then True else False
--é equivalente a: x = A
--desconta por a primeira coisa

--80, 81, 85, 86, 87
