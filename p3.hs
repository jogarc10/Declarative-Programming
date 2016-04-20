--Exer 1
data Direccion = Arriba | Abajo| Izquierda | Derecha deriving (Eq,Ord,Show)

mueve :: (Num t1, Num t) => [Direccion] -> (t, t1) -> (t, t1)
mueve [] (a,b) = (a,b)
mueve (x:xs) (a,b)
	| x == Arriba = mueve xs (a,b+1)
	| x == Abajo = mueve xs (a,b-1)
	| x == Izquierda = mueve xs (a-1,b)
	| x == Derecha = mueve xs (a+1,b)

trayectoria::(Num t1, Num t) => [Direccion] -> (t, t1) -> [(t, t1)]
trayectoria [] (a,b) = [(a,b)]
trayectoria (x:xs) (a,b)
	| x == Arriba = (a,b):trayectoria xs (a,b+1)
	| x == Abajo = (a,b):trayectoria xs (a,b-1)
	| x == Izquierda = (a,b):trayectoria xs (a-1,b)
	| x == Derecha = (a,b):trayectoria xs (a+1,b)

inferior (x:xs) (y:ys) = inferiorAux (trayectoria (x:xs) (0,0)) (trayectoria (y:ys) (0,0))

inferiorAux [] _ = True
inferiorAux _ [] = True
inferiorAux ((_,b):xs) ((_,d):ys)
	| b > d = False
	| otherwise = inferiorAux xs ys

mul dir 0 = []
mul dir x = dir:(mul dir (x-1))


--Exer 2
--data Complejo = 