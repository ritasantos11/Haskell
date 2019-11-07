
h (x:xs) = (2*x) : h xs
h [x] = []
h [] = []


sum xs = foldr (+) 0 xs

--sum [1,2,3] = 1+2+3+0

--foldr: 1+(2+(3+0)) = 1+(2+3) = 1+5 = 6

--Exame de Recurso 1516
scanrola f v [] = [v]
scanrola f v (x:xs) = (f x y) : (y:ys)
                    where (y:ys) = scanrola f v xs

scanrola1 f v xs = foldr g v xs
                 where g x (y:ys) = (f x y) : (y:ys)

maiores:: Ord a => [a] -> Bool
maiores xs = and (zipWith (>) xs (tail xs))

data Arv a = Vazia | No a (Arv a) (Arv a)
           deriving (Show, Eq)

arv1:: Arv Int
arv1 = No 15 (No 10 (No 5 Vazia Vazia) Vazia) (No 20 Vazia Vazia)

valorArv:: Num a => Arv a -> a
valorArv Vazia = 0
valorArv (No x _ _) = x

somasTree:: Num a => Arv a -> Arv a
somasTree Vazia = Vazia
somasTree (No x esq dir) = No (x + valorArv sesq + valorArv sdir) sesq sdir
                         where sesq = somasTree esq
                               sdir = somasTree dir



{-data E a = C a | D (E a) (E a)

eval:: E a -> (a -> a -> a) -> a
eval (C c) f = c
eval (D x y) f = f (eval x f) eval y f
-}
