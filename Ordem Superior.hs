# Funcoes de ordem superior em Haskell
## Aulas mbb

### Padrao `map`

Vejamos estas funcoes em Haskell que aplicam a mesma operacao a todos os elementos de uma lista:

	pares :: [Int] -> [Bool]
	pares [] = []
	pares (x:xs) = (x `mod` 2 == 0):pares xs

	soma1 :: [Int] -> [Int]
	soma1 [] = []
	soma1 (x:xs) = (x+1):soma1 xs

Se quisermos poupar trabalho podemos escrever uma unica funcao que pode ser utilizada para escrever todas as funcoes que seguem este padrao:

	repeteop :: (a -> b) -> [a] -> [b]
	repeteop _ [] = []
	repeteop f (x:xs) = (f x):repeteop f xs

As funcoes podem assim ser reescritas como:

	pares' l = repeteop (\x -> x `mod` 2 == 0) l

	soma1' l = repeteop (\x -> x+1) l

Esta funcao `repeteop` correspode exactamente ao `map`:

	pares'' l = map (\x -> x `mod` 2 == 0) l

	soma1'' l = map (\x -> x+1) l

### Padrao `filter`

Vejamos estas funcoes em Haskell que escolhem alguns elementos de uma lista de acordo com um determinado teste:

	sopares :: [Int] -> [Int]
	sopares [] = []
	sopares (x:xs) | x `mod` 2 == 0 = x:sopares xs 
                | otherwise      = sopares xs

	soimpares :: [Int] -> [Int]
	soimpares [] = []
	soimpares (x:xs) | x `mod` 2 == 1 = x:soimpares xs 
                | otherwise      = soimpares xs

Se quisermos poupar trabalho podemos escrever uma unica funcao que pode ser utilizada para escrever todas as funcoes que seguem este padrao:

	soosqueeugosto :: (a->Bool) -> [a] -> [a]
	soosqueeugosto _ [] = []
	soosqueeugosto gosto (x:xs) | gosto x = x:soosqueeugosto gosto xs
	                            | otherwise = soosqueeugosto gosto xs

As funcoes podem assim ser reescritas como:

	sopares' l = soosqueeugosto (\x -> x `mod` 2 == 0) l

	soimpares' l = soosqueeugosto (\x -> x `mod` 2 == 1) l

Esta funcao `soosqueeugosto` correspode exactamente ao `filter`:

	sopares'' l = filter (\x -> x `mod` 2 == 0) l

	soimpares'' l = filter (\x -> x `mod` 2 == 1) l

### Padrao `foldr`

Vejamos estas funcoes em Haskell que processam os elementos de uma lista, combinando-os da direita para esquerda (do final para o inicio) ate obter um resultado final:

	somatodos :: [Int] -> Int
	somatodos [] = 0
	--somatodos (x:xs) = x + somatodos xs
	somatodos (x:xs) = f x (somatodos xs)
	       where
	         f x y = x + y

	todospos :: [Int] -> Bool
	todospos [] = True
	--todospos (x:xs) = (x > 0) && todospos xs
	todospos (x:xs) = f x (todospos xs)
	      where 
	        f x y = (x > 0) && y
 
Se quisermos poupar trabalho podemos escrever uma unica funcao que pode ser utilizada para escrever todas as funcoes que seguem este padrao:

	juntadiresq :: (a -> b -> b) -> b -> [a] -> b
	juntadiresq _ y0 [] = y0
	juntadiresq f y0 (x:xs) = f x (juntadiresq f y0 xs)

As funcoes podem assim ser reescritas como:

	somatodos' l = juntadiresq f 0 l
	       where
	         f x y = x + y

	todospos' l = juntadiresq f True l
	       where
	         f x y = (x > 0) && y                          

Esta funcao `juntadiresq` correspode exactamente ao `foldr`:

	somatodos'' l = foldr f 0 l
	       where
	         f x y = x + y

	todospos'' l = foldr f True l
	       where
	         f x y = (x > 0) && y  

Em geral para se escrever uma funcao utilizando o `foldr` pode comecar por escrever-se a funcao de acordo com o seguinte template:

	nomefunc :: [a] -> b
	nomefunc [] = ??????
	nomefunc (x:xs) = f x (todospos xs)
	      where 
	        f x y = ?????

Em que os pontos de interrogacao correspondem aos pontos deixados em aberto pelo `foldr`.

### Padrao `foldl`

O padrao `foldl` corresponde ao processo de processar os elementos de uma lista do inicio para o fim (da esquerda para a direita):

	juntaesqdir :: (b -> a -> b) -> b -> [a] -> b
	juntaesqdir _ y0 [] = y0
	juntaesqdir f y0 (x:xs) = juntaesqdir f (f y0 x) xs

Em geral para se escrever uma funcao utilizando o `foldl` pode comecar por escrever-se a funcao de acordo com o seguinte template:

	nomefunc :: [a] -> [a]
	nomefunc l = aux ???? l
	    where 
	        aux y0 [] = y0
	        aux y0 (x:xs) = aux (f y0 x) xs
	           where
	             f ac cabeca = ???????

Mais uma vez os pontos de interrogacao correspondem aos parametros da funcao `foldl`: o valor inicial que vais ser combinado com o primeiro elemento da lista, e a funcao que combina o valor acumulado com o proximo elemento da lista.

