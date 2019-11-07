-- 1
-- a) [6, 9]
-- b) ["abc", "", "dce"]
-- c) 4
-- d) [0+1, 2+3, 4+5, 6+7, 8+9]
-- e) [False, False, True, True, True]
-- f) (True == (False == (True == False))) = True
-- g) [x*(-1)^x | x <- [2..11]]
-- h) h = f (3,2,1) = f [2,1] -3 = (f [1] -2) -3 = ((f[] -1) -2) -3 = ((0 -1) -2) -3 = -6
--h = f [3,2,1]
  --where f (y:z) = f z - y
    --    f _ = 0

-- i) [Int] -> [Int]
-- j) [id, not] :: [Bool -> Bool]
-- k) eval:: [a -> (a -> a -> a) -> a]
-- l) f::[a] -> Int -> a

-- 2
-- a)
somavalor:: [Int] -> Int -> [Int]
somavalor xs x = map (+x) xs
-- OU somavalor [x+y | y <- xs]

-- b)
maiorque:: Ord a => a -> [a] -> Bool
maiorque x xs = and (map (x>) xs)
-- ou maiorque xs = filter (x>) xs == xs

-- 3
-- a)
matid:: Int -> [[Int]]
matid n = [[if x==y then 1 else 0 | y <- [0..n-1]] | x <- [0..n-1]]

-- b)
simetrica:: Eq a => [[a]] -> Bool
simetrica xs = and [(xs !! x) !! y == (xs !! y) !! x | x <- [0..n-1], y <- [x..n-1]]
             where n = length xs

-- 4
-- a)
strings:: [String]
strings = "" : [y:x | x <- strings, y <- ['a'..'z']]
 --OU
 -- strings = "" : [x++[y] | x <- strings, y <- ['a'..'z']]

-- b
menores:: Int -> [String]
menores n = takeWhile (\x -> length x<n) strings
-- OU
-- menores n = takeWhile f strings
--           where f x = length x<n

 -- 5
data Arv a = Folha a
           | No (Arv a) (Arv a)
 -- a)
soma:: Num a => Arv a -> a
soma (Folha x) = x
soma (No esq dir) = soma esq + soma dir

-- b)
listar:: Arv a -> [a]
listar (Folha x) = [x]
listar (No esq dir) = listar esq ++ listar dir
