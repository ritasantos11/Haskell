
--tabuada:: Int -> IO()
--tabuada = do n <- getChar
--             if mod x n == 0 then putStrLn x

maiorQ:: [Int] -> Int -> [Int]
maiorQ xs x = [y | y <- xs, y > x]

tamanhoS:: [String] -> [Int]
tamanhoS xs = [length x | x <- xs]

f (x:xs) = (2+x) * f xs
f [x] = 3+x
f [] = 1


ola:: [Int] -> Int
ola x = x!!3

