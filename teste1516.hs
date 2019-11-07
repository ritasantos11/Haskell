-- 1.
-- a) [2,3,1,4,4]
-- b) [0,10,20,30,40]
-- c) [[1,2], [], [3,4], [5]]
-- d) 5 (é o elemento que está no indice 3)
-- e) [1,1,1,1,1,1] (temos que ver o x<-[1..3] e y<-[1..x] como for's)
-- f) [(1,4), (2,3), (3,2)]
-- g) [2^x | x<-[0..6]]
-- h) 5*4*3*2*1*0=0
-- i) Num t => [(Bool, t)] (Num t = t pode tomar qualquer valor numerico)
-- j) troca:: (a,b) -> (b,a) (tipo geral)
-- k) Int -> Int -> Int (tipo admissivel)
-- l) [([a], a)] (lista de a's)

-- 2.
-- a) 
triangulo:: Int -> Int -> Int -> String
triangulo a b c | a == b && a == c && b == c = "equiláteros"
                | a == b || a == c || b == c = "isósceles"
                | otherwise = "escalenos"
-- b)
retangulo:: Int -> Int -> Int -> Bool
retangulo x y z = x^2==y^2+z^2 || y^2==x^2+z^2 || z^2==x^2+y^2

-- 3.
maiores:: Ord a => [a] -> [a]
maiores [] = []
maiores [x] = []
maiores (x:y:xs) | x>=y = x:maiores (y:xs)
                 | otherwise = maiores (y:xs)

maiores2:: Ord a => [a] -> [a]
maiores2 xs = [x | (x,y) <- zip xs (tail xs), x>=y]

-- 4.
-- a)
somapares:: Num a => [(a,a)] -> [a]
somapares [] = []
somapares ((x,y):xs) = (x+y) : somapares xs

-- b)
somapares2:: Num a => [(a,a)] -> [a]
somapares2 xs = [x+y | (x,y) <- xs]

-- 5.
-- a)
itera:: Int -> (a ->  a) -> a -> a --(a = tipo)
itera 0 f v = v
itera n f v = itera (n-1) f (f v)
-- ou itera n f v = f itera (n-1) f v

-- b)
mult x y = itera x (+y) 0