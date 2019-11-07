--1
inc:: Int -> Int
inc x = x + 1

quadrado:: Int -> Int
quadrado x = x * x

dobro:: Int -> Int
dobro x = x + x

media:: Float -> Float -> Float
media x y = (x + y) / 2

--2
triangulo:: Int -> Int -> Int -> Bool
triangulo a b c | a + b > c && a + c > b && b + c > a = True
                | otherwise = False

--3
area:: Float -> Float -> Float -> Float
area a b c = sqrt (s * (s - a) * (s - b) * (s - c))
           where s = (a + b + c) / 2

--4
metades:: [Int] -> ([Int], [Int])
metades a = (take s a, drop s a)
          where s = (length a) `div` 2  -- = div (length a) 2

--5 a)
last1:: [Int] -> [Int]
last1 a = drop x a
       where x = (length a) - 1

--5 b)
init1:: [Int] -> [Int]
init1 a = take x a
        where x = (length a) - 1

--6
binom:: Int -> Int -> Int
binom n k = a `div` (b*c)
          where a = product [1 .. n]
                b = product [1 .. k]
                c = product [1 .. (n - k)]

--7
max3:: Int -> Int -> Int -> Int
max3 x y z | x > y && x > z = x
           | y > x && y > z = y
           | otherwise = z

min3:: Int -> Int -> Int -> Int
min3 x y z | x < y && x < z = x
           | y < x && y < z = y
           | otherwise = z

mid3:: Int -> Int -> Int -> Int
mid3 x y z | (x < y && x > z) || (x > y && x < z) = x
           | y < x && y < z = y
           | otherwise = z
           
--8 a)
maxOccurs:: Int -> Int -> (Int, Int)
maxOccurs x y = (if x > y then x else y, if x == y then 2 else 1)

--b)
orderTriple:: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (x, y, z) = (min3 x y z, mid3 x y z, max3 x y z)

--9
classifica:: Int -> String
classifica n | n <= 9 = "reprovado"
             | n > 10 && n < 12 = "suficiente"
             | n > 13 && n < 15 = "bom"
             | n > 16 && n < 18 = "muito bom"
             | n > 19 && n < 20 = "muito bom com distinção"

--10
xor:: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

--11
safetail:: [a] -> [a]
safetail xs = if null xs then [] else tail xs

--usando padrões
safetail1:: [a] -> [a]
safetail1 [] = []
safetail1 (x:xs) = xs

safetail2:: [a] -> [a]
safetail2 l | null l = []  --null verifica se a lista é vazia
            | otherwise = tail l

--12 b)
curta:: [a] -> Bool
curta [] = True
curta [x] = True
curta [x,y] = True  --só 2 elementos
curta (x:xs) = False  --lista indefenida. não é preciso ser assim (x:y:xs), porque lê por ordem
--ou
curta3 _ = False

--12 a)
curta1 l = length (l) < 3
--ou
curta2 l | length l <= 2 = True

--22
soma:: [Int] -> Int
soma x = sum ([x^2 | x <- [1..100]])

--23 a)
aprox:: Int -> Double --x é um Int e tem de passar a ser um Double
aprox n = sum ([ (-1) ^ (fromIntegral x) / (2 * (fromIntegral x) + 1) | x <- [0..n]])

--23 b)
aprox':: Int -> Double
aprox' k = sum ([ (-1) ^ (fromIntegral x) / ((fromIntegral x) + 1) ^ 2 | x <- [0..k]])

--24
divprop:: Int -> [Int]
divprop n = [x |  x <- [1..n-1], mod n x == 0]

--25
prefeitos:: Int -> [Int]
prefeitos n = [x | x <- [1..n], sum (divprop x) == x]

--26
primo:: Int -> Bool
primo n = length (divprop n) + 1 == 2

--27
pascal:: Int -> [[Int]]
--1 lista de compreensão que são as linhas. Outra que são as colunas
pascal n = [[binom x k | k <- [0..x]] | x <- [0..n]]

--28
dotprod:: [Int] -> [Int] -> Int
dotprod a b = sum (zipWith (*) a b)

dotprod1:: [Int] -> [Int] -> Int
dotprod1 a b = sum [x*y | (x,y) <- (zip a b)]

--29
pitagoricos:: Int -> [(Int,Int,Int)]
pitagoricos n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

--30 a)
factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--30 b)
rangeProduct:: Int -> Int -> Int
rangeProduct m n | m == n = n
                 | otherwise = n * rangeProduct m (n - 1)

--30 c)
factorial2:: Int -> Int
factorial2 0 = 1
factorial2 n = rangeProduct 1 n

--31
multi:: Int -> Int -> Int
multi n 0 = 0
multi n 1 = n
multi n m = n + multi n (m - 1)

--32
--3 é o número que ao quadrado fica mais perto de 15
raiz:: Int -> Int
raiz n = head (reverse y)
       where y = [x | x <- [1..n], x^2 <= n] 

--Integer: vários tipos de Inteiros. Valores muito grandes. Como o Float e o Double

--33
maxf:: (Integer -> Integer) -> Integer -> Integer
--Ex: prelude> maxf (+1) 5
--Ex2: prelude> maxf (\x -> x ^ 2 - 5) 7
maxf f 0 = f 0
maxf f n = max (maxf f (n-1)) (f n)

--34
boolf:: (Integer -> Integer) -> Integer -> Bool
boolf f 0 = f 0 == 0
boolf f n = if (f n)==0 then True else boolf f (n-1)

--35
somaf:: (Integer -> Integer) -> Integer -> Integer
somaf f 0 = f 0
somaf f n = f n + somaf f (n-1)

--36
maxdiv:: Int -> Int -> Int
maxdiv n 0 = n
maxdiv n m = maxdiv m (mod n m) --Algoritmo de Euclides

--Definir recursivamente: 1º caso para o 0. 2º caso para n
--Para as listas o caso 0 é a lista vazia

--38 a)
and1:: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && and1 xs

--38 b)
or1:: [Bool] -> Bool
or1 [] = True
or1 (x:xs) = x || or1 xs

--38 c)
concat1:: [[a]] -> [a]
concat1 [] = []
--concatx ([1,2,3],[7,4,2],[2,3,9])
--concat [1,2,3] ++ [7,4,2,2,3,9]
concat1 (x:xs) = x ++ concat1 xs --o x é uma lista

--38 d)
replicate1:: Int -> a -> [a]
--vai repetir o k n vezes
replicate1 0 k = []
replicate1 n k = (k:replicate1 (n-1) k)

--38 e)
sel:: [a] -> Int -> a
sel (x:xs) 0 = x
sel (x:xs) n = sel xs (n-1)

--38 f)
elem1:: Eq a => a -> [a] -> Bool
elem1 n [] = False
elem1 n (x:xs) = x == n || elem1 n xs

--40
forte:: String -> Bool
forte x = not(null [a | a <- x, a >= 'a' && a  <= 'z'] || null [b | b <-x, b >= 'A' && b <= 'Z'] || null [c | c <- x, c >= '0' && c <= '9'] || length x < 8)

--42
interperse2:: a -> [a] -> [a]
interperse2 c [] = []
interperse2 c (x:xs) | length (x:xs) > 1 = (x:c:interperse2 c xs)
                     | otherwise = [x]

--l == []  l precisa de estar na classe equal
--null l da para qualquer tipo de l

--43 a)
insert:: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n < x = (n:x:xs)
                | otherwise = (x:insert n xs)

--43 b)
isort:: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

--44 a)
minimum1:: Ord a => [a] -> a
minimum1 [] = undefined
minimum1 [x] = x
minimum1 (x:xs) | x < (minimum1 xs) = x
                | otherwise = minimum1 xs

--44 b)
delete:: Eq a => a -> [a] -> [a]
delete  n [] = []
delete n (x:xs) | n==x = xs
                | otherwise = (x:delete n xs)

--44 c)
ssort:: Ord a => [a] -> [a]
ssort [] = []
ssort (x:xs) = minimum1 (x:xs) : ssort (delete (minimum (x:xs)) (x:xs))

--45 a)
merge:: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] (y:ys) = ssort (y:ys)
merge (x:xs) [] = ssort (x:xs)
merge (x:xs) (y:ys) = ssort ((x:xs) ++ (y:ys))

--45 b)
msort:: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort (x:xs) = merge a b
             where a = take n (x:xs)
                   b = drop n (x:xs)
                   n = div (length (x:xs)) 2
