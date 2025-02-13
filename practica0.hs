module Practica0 where 


{-- Recursion y recursion de Cola --}

--Funcion buscar : Dada una lista de Enteros y elemento , Regresa verdadero en caso de que el elemento se encuentre en la lista
--En otro caso regresa False 

--Implementando la busqueda con recursion de cola

buscar :: [Int] -> Int -> Bool
buscar xs elemento = buscar_aux xs elemento False
  where
    buscar_aux :: [Int] -> Int -> Bool -> Bool
    buscar_aux [] _ acc = acc  
    buscar_aux (x:xs) elemento acc
      | x == elemento = True                
      | otherwise     = buscar_aux xs elemento acc

--Funcion sumar_lista : Dada una Lista de Entero , regresa la suma de sus elementos
--Implementala con recursion de Cola
sumar_lista::[Int]->Int
sumar_lista xs = sumar_aux xs  0
 where 
    sumar_aux :: [Int] -> Int -> Int 
    sumar_aux [] acc     = acc  
    sumar_aux (x:xs) acc = sumar_aux xs (acc + x)


--Implementa una funcion recursiva de forma "ordinaria" y despues implementala con recursion de cola
--Usa los comandos vistos en clase para comparar tiempo y memoria usados y dado el resultado describe que sucedio
--Y porque crees que haya sido asi
-- :s +t (en el ghci  para ver la estadisticas )

buscar_ordinaria::[Int]->Int->Bool
buscar_ordinaria [] _ = False
buscar_ordinaria (x:xs) elemento
  | x == elemento    = True
  | otherwise = buscar xs elemento

--funcion de sumar_lista de forma ordinaria
sumar_lista_ordinaria :: [Int] -> Int
sumar_lista_ordinaria [] = 0  
sumar_lista_ordinaria (x:xs) = x + sumar_lista_ordinaria xs

--
{--funciones--}

--Funcion filter toma un predicado (funcion que regresa booleano) y filtra los elementos la lista de entrada  dada la condicion
filterB:: (a -> Bool) -> [a] -> [a]
filterB _ [] = []
filterB p (x:xs)
  | p x       = x : filterB p xs  
  | otherwise = filterB p xs

--Implementa una funcion llamada mapear que reciba como argumento una funcion y aplique esta funcion a una lista
mapear:: (a->b) -> [a] -> [b]
mapear _ [] = [] 
mapear f (x:xs) = f x : mapear f xs


--Decima extra : .2
--Forma comprehension
mapear_:: (a->b) -> [a] -> [b]
mapear_ f list = [f x | x <- list]




{--Tipos,clases y Estructuras de Datos --}

--Arbol 
data Tree a = Empty 
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)
 


--Dada la definicion de arbol binario has una funcion que haga un recorrido pre order
preorder:: Tree a -> [a]
preorder Empty =  []
preorder (Node root left right) =  [root] ++  preorder left ++ preorder right

--Hacer una funcion que calcule la altura del arbol ,regresa verdadero en caso de encontrar el eelemento en el arbol
buscar_tree:: Eq a => Tree a -> a -> Bool
buscar_tree Empty _  =  False
buscar_tree arbol x = let elementos = preorder arbol in
                      x `elem` elementos


--Punto Extra:  Implementa  una funcion que cuente la cantidad de hojas del arbol 
hojas:: Tree a -> Int
hojas Empty  = 0
hojas (Node raiz Empty Empty) = 1
hojas (Node raiz izq der) = hojas izq + hojas der


--Definicion de Grafica 

type Vertex = Int
type Graph = [(Vertex, [Vertex])]

vecinos :: Graph -> Vertex -> [Vertex]
vecinos [] _ = []  
vecinos ((v, ns):xs) x
    | v == x    = ns 
    | otherwise = vecinos xs x

dfs :: Graph -> Vertex -> [Vertex] -> [Vertex]
dfs graph v visited
    | v `elem` visited = visited  
    | otherwise = foldl (\acc n -> dfs graph n acc) (v : visited) (vecinos graph v)

--Dada la siguiente defincion de grafica , crea una funcion que verifique si la grafica es conexa 
--Tip: USA la funcion auxiliar dfs, (si quieres puedes usar otra de tu propio diseño)



isConnected :: Graph -> Bool   --Funcion a Implementar
isConnected [] = True
isConnected ((x,vs):xs) = let vertices = dfs ((x,vs):xs) x [] in
                   length ((x,vs):xs) == length vertices

--Ejemplos

connectedGraph :: Graph
connectedGraph = [(1, [2,3]), (2, [4]), (3, [4,5]), (4, [6]), (5, [6]), (6, [])]  --Debe Regresar True

disconnectedGraph :: Graph
disconnectedGraph = [(1, [2]), (2, [1]), (3, [4]), (4, [3])] --Debe regresar False 

tree :: Graph
tree = [(1, [2,3]),(2, [4,5]), (3, [6,7]), (4, []), (5, []), (6, []), (7, [])]


--La siguiente funcion verfiica que la grafica es un arbol 
--Tip : Recuerda que un arbol es una grafica conexa y sin ciclos
isTree :: Graph -> Bool
isTree []  = True
isTree ((x,vs):xs) = isConnected ((x,vs):xs) && not (hayCiclos ((x,vs):xs) [] [x])

hayCiclos :: Graph -> [Vertex] -> [Vertex] -> Bool
hayCiclos _ _ [] = False
hayCiclos grafica visitados (x:xs) = x `elem` visitados || hayCiclos grafica (x:visitados) (xs ++ vecinos grafica x)


--La siguiente funcion regresa a suma de las hojas del arbol
leafSum:: Tree Int -> Int 
leafSum Empty = 0
leafSum arbol = let lista = preorder arbol in
                sumar_lista lista