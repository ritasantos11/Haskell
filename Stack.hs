module Stack (Stack, push, pop, top, empty, isEmpty) where
data Stack a = Stk [a]

--acresencenta um valor ao topo da pilha
push:: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)
--como o null das listas(?)

--remove o valor do topo da lista
pop:: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"
--como o tail das listas

--obtém o valor do topo da lista
top:: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"
--como o head das listas

--cria uma pilha vazia
empty:: Stack a
empty = Stk []

--verifica se a pilha é vazia
isEmpty:: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False


makeStack:: [a] -> Stack a
makeStack xs = foldr push empty xs

size:: Stack a -> Int
size s | isEmpty s = 0
       | otherwise = 1 + size (pop s)

--79
parent:: String -> Bool
parent str = parentAux str empty

parentAux:: String -> Stack Char -> Bool
parentAux [] stk = isEmpty stk
parentAux ('(':xs) stk = parentAux xs (push ')' stk)
parentAux('[':xs) stk = parentAux xs (push ']' stk)
parentAux('{':xs) stk = parentAux xs (push '}' stk)
parentAux (x:xs) stk = not (isEmpty stk) && x == top stk && parentAux xs (pop stk)
