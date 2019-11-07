--readFile:: FilePath -> IO String

--writeFile:: FilePath -> String -> IO()

--appearFile:: FilePath -> String -> IO()


{-module Main where
main:: IO()
main = do x <- getChar
          putChar x
          if x == '\n' then return () else main
-}

{-boasVindas:: IO() 
boasVindas = do putStrLn "Como te chamas? "
                nome <- getLine
                putStrLn ("Bem-vindo, " ++ nome ++ "!")
-}

import Data.Char(isDigit)
import System.Random(randomRIO)
main = do x <- randomRIO (1,100) -- escolher número aletório
          n <- jogo 1 x -- começar o jogo
          putStrLn ("Acertou em " ++ show n ++
                    " tentativas")

jogo :: Int -> Int -> IO Int
jogo n x
  = do { putStrLn "Tentativa? "
       ; str <- getLine
       ; if all isDigit str then
           let y = read str in
           if y>x then
             do putStrLn "Demasiado alto!"; jogo (n+1) x
           else if y<x then
             do putStrLn "Demasiado baixo!"; jogo (n+1) x
                else return n
         else do putStrLn "Tentativa inválida!"; jogo n x
       }
