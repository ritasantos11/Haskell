import Data.Char

let2int:: Char -> Int
let2int x = ord x - ord 'a'

int2let:: Int -> Char
int2let n = chr (n + ord 'a')

minuscula:: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'

deslocar:: Int -> Char -> Char
deslocar k x | minuscula x = int2let (mod (let2int x + k) 26)
             | otherwise = x

cifrar:: Int -> String -> String
cifrar k xs = [deslocar k x | x <- xs]
