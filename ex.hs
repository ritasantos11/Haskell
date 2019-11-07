--EXEMPLOS:

--obter quadrados dos números pares até  n
qpares:: Int -> [Int]
qpares n = [x^2 | x <- [1..n], mod x 2 == 0]

--obter números ímpares da lista dada
limpares:: [Int] -> [Int]
limpares xs = [x | x <- xs, mod x 2 /= 0]



--EXERCICIOS:

--4
metades:: [a] -> ([a], [a])
metades xs = (take a xs, drop a xs)
           where a = div (length xs) 2

--6
binom:: Int -> Int -> Int
binom n k = div a (b * c)
          where a = product [1..n]
                b = product [1..k]
                c = product [1..(n-k)]


max1 x y = if x >= y then x else y

iguais x y = if x == y then 2 else 1

--8
maxOccurs:: Integer -> Integer -> (Integer, Integer)
maxOccurs a b = (max1 a b, iguais a b)

--11
safetail:: [a] -> [a] 
safetail xs = if length xs == 0 then [] else tail xs

safetail1:: [a] -> [a]
safetail1 [] = []
safetail1 (x:xs) = xs

safetail2:: [a] -> [a]
safetail2 xs | length xs == 0 = []
             | otherwise = tail xs

--12 b)
curta:: [a] -> Bool
curta [] = True
curta [x] = True
curta [x,y] = True
curta _ = False

--12 a)
curta1:: [a] -> Bool
curta1 xs | length xs == 0 = True
          | length xs == 1 = True
          | length xs == 2 = True
          | otherwise = False

--22
suml:: Int
suml = sum [x^2 | x <- [1..100]]

--23
aprox:: Int -> Double --para passar um inteiro para um double: fromIntegral
aprox x = sum [(-1) ^ (fromIntegral n) / (2*(fromIntegral n) + 1) | n <- [0..x]]

--24
divprop:: Int -> [Int]
divprop n = [x | x <- [1..n-1], mod n x == 0]

--25
perfeitos:: Int -> [Int]
perfeitos n = [x | x <- [1..n-1], x == sum (divprop x)]

--26
primo:: Int -> Bool
--divprop dá uma lista
primo n = divprop n == [1, n]

--é primo se os únicos divisores for o 1 e o próprio númer
--o divprop nao vai até ao n (número dado), vai até ao n-1

--ou

primo1:: Int -> Bool
primo1 n = length (divprop n) == 1

--27
pascal:: Int -> [[Int]]
pascal n = [[binom x k | k <- [0..x]] | x <- [0..n]]

--28
dotprod:: [Float] -> [Float] -> Float
dotprod xs ys = sum [x*y | (x,y) <- zip xs ys]

--29
pitagoricos:: Int -> [(Int,Int,Int)]
pitagoricos n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--30 a)
fatorial:: Int -> Int
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

--33
maxf:: (Integer -> Integer) -> Integer -> Integer
maxf f 0 = f 0
maxf f n = f n

--34
--verificar se algum valor de f 0 ... f n é igual a 0
boolf:: (Integer -> Integer) -> Integer -> Bool
boolf f n = f n == 0

--35
somaf:: (Integer -> Integer) -> Integer -> Integer
somaf f 0 = f 0
somaf f n = f n + somaf f (n-1)

--36
{-mdc:: (Int,Int) -> Int
mdc (a,b) | b==0 = a
          | otherwise = mdc (b, mod a b)
-}

--ou
mdc :: Integral a => a -> a -> a
mdc a b | mod a b == 0 = b
        | mod b a == 0 = a
        | a > b = mdc b (mod a b)
        | a < b = mdc a (mod b a)

--37
--calcula 2^n
potf:: Int -> Int
potf 0 = 1
potf n = 2^n

--38 a)
andx:: [Bool] -> Bool
andx [] = True
andx (x:xs) = x && andx xs

--38 b)
orx:: [Bool] -> Bool
orx [] = True
orx (x:xs) = x  || orx xs

--38 c)
concatx:: [[a]] -> [a]
concatx [] = []
concatx (x:xs) = x ++ concatx xs --o x é uma lista

--38 d)
replicatex:: Int -> a -> [a]
replicatex 0 k = []
replicatex n k = (k:replicatex (n-1) k)

--42
interperse1:: a -> [a] -> [a]
interperse1 n [] = []
interperse1 n [x] = [x]
interperse1 n (x:xs) = x:n:interperse1 n xs

--43 a)
insert:: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n < x = (n:x:xs)
                | otherwise = x:insert n xs

--43 b)
isort:: Ord a => [a] -> [a]
ĩsort [] = []
isort (x:xs) = insert x (isort xs)

--44 a)
minimum1:: Ord a => [a] -> a
minimum1 [] = undefined
minimum1 [x] = x
minimum1 (x:xs) | x < minimum1 xs = x
                | otherwise = minimum1 xs

--44 b)
delete:: Eq a => a -> [a] -> [a]
delete n [] = []
delete n (x:xs) | n == x = xs
                | otherwise = (x:delete n xs)

--44 c)
ssort:: Ord a => [a] -> [a]
ssort [] = []
ssort xs = (b : ssort (delete b xs))
         where b = minimum xs

--45 a)
merge:: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = ssort xs
merge [] ys = ssort ys
merge xs ys = ssort (xs ++ ys)            

--48
mapfilter:: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapfilter f p xs = map f (filter p xs)

--51
zipWith1:: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f [] [] = []
zipWith1 f [] (y:ys) = []
zipWith1 f (x:xs) [] = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

data Arv a = Vazia
           | No a (Arv a) (Arv a)
           deriving (Show, Eq)

arv1:: Arv Int
arv1 = No 11 (No 5 Vazia (No 7 Vazia Vazia)) ( No 12 Vazia (No 20 Vazia Vazia))

--70
sumArv:: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No x esq dir) = x + sumArv esq + sumArv dir

listar:: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = [x] ++ listar esq ++ listar dir

--71
ldecrescente:: Arv a -> [a]
ldecrescente Vazia = []
ldecrescente (No x esq dir) = ldecrescente dir ++ [x] ++ ldecrescente esq

--74
mapArv:: (a -> b) -> Arv a -> Arv b
mapArv f Vazia = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)



somavalor:: [Int] -> Int -> [Int]
somavalor [] n = []
somavalor (x:xs) n = (x+n) : somavalor xs n


--Exemplos:
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (+) 0 xs

reverse2:: [a] -> [a]
reverse2 xs = foldl (\x y -> (y:x)) [] xs

reverse3 xs = foldr (:) [] (reverse xs)

concat1 xs ys = foldr (\x y -> x:y) ys xs

impares n = map(\x -> x*2 + 1) [0..(n-1)]

reverse1:: [a] -> [a]
reverse1 xs = foldl (\x y -> [y] ++ x) [] xs

shift:: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

boasVindas:: IO() 
boasVindas = do putStrLn "Como te chamas? "
                nome <- getLine
                putStrLn ("Bem-vindo, " ++ nome ++ "!")


somavalor1:: [Int] -> Int -> [Int]
somavalor1 [] n = []
somavalor1 (x:xs) n = (n+x) : somavalor xs n