millionYearsToSec x = x*1000000*365*24*60*60*1000/1

tupleSecsTo x = (secToYears x, remainingDays x, remainingHours x, remainingMin x, remainingSecs x)

secToYears x = div (div (div (div x 60) 60) 24) 365

remainingDays x = (div(div(div x 60) 60) 24) `mod` 365

remainingHours x = (div(div x 60) 60) `mod` 24

remainingMin x = (div x 60) `mod` 60 

remainingSecs x = x `mod` 60

--ejer 2
--2.1
--f x y = 2*x-y*x
--g x = f(f 2 x) (f x 1)
--h x y z = f((f(x+2y) g 3) (5 - g z - y))
--i x y = if x >= y && y > 0 then x - y
  --else if 0 < x && x < y then 0
  --else y - x

--2.2
f x y = (-)((*) 2 x) ((*) y x)
g x = f(f 2 x) (f x 1)
h x y z = f((f((+) x 2y) g 3) ((-)((-) 5 (g z)) y))
i x y = if x >= y && y > 0 then (-) x y
  else if 0 < x && x < y then 0
  else (-) y x

--ejer 3
--3.1
--tresIguales x y z = (x==y) && (y==z) && (z==x)

--distintos x y z = (x/=y) && (y/=z) && (z/=x)

--3.2
tresIguales x y z = (&&)((&&) ((==) x y) ((==) y z)) ((==) z x)

distintos x y z = (&&)((&&) ((/=) x y) ((/=) y z)) ((/=) z x)

--ejer 4
--4.1
{-y False _ = False
y _ z = z

o True _ = True
o _ z = z

no True = False
no _ = True

imp False _ = True
imp _ z = z

xor False z = z
xor _ z = no z-}

--4.2
{-False `y` _ = False
_ `y` z = z

True `o` _ = True
_ `o` z = z

False `imp` _ = True
_ `imp` z = z

False `xor` z = z
_ `xor` True = False
_ `xor` _ = True-}

--4.3
o x y = not(not x && not y)

imp x y = not(x && not y)

xor x y = not(not(x && not y) && not(not x && y))

--Exer 5
--5.1
yy True False = False
yy True _ = True
yy False _ = False

oo False x = x
oo True _ = True

--5.2 was just checking

--5.3 I do not get the exercise

--Exer 6
media :: (Fractional a) => [a] -> a
media x =(sum x) / (len x)


len [] = 0
len (x:xs) = 1 + len xs

--Exer 7






------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------HOJA 2-----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------

--Exer 2
filter2:: [a] ->(a -> Bool)->(a -> Bool) -> ([a],[a])
filter2 [] p q = ([] , [])
filter2 (x:xs) p q 
   | p x && q x = (x:a, x:b)
   | p x = (x:a, b)
   | q x = (a, x:b)
   | otherwise = (a, b)
   where (a,b) = filter2 xs p q


filters:: [a] ->[(a -> Bool)]->[a]
filters [] [] = []
filters [] (x:xs) = []
filters (x:xs) (y:ys)
   | y x = x:filters xs ys
   | otherwise = filters xs ys


partition::(a->Bool)->[a]->([a],[a])
partition p [] = ([] , [])
partition p (x:xs)
   | p x = (x:a,b)
   | otherwise = (a,x:b)
    where(a,b) = partition p xs


spam::(a->Bool)->[a]->([a],[a])
spam p [] = ([],[])
spam p (x:xs)
   | p x = (x:a, b)
   |otherwise = ([],xs)
   where(a,b) = span p xs

iguales:: (Ord a1, Ord a, Enum a) => (a -> a1) -> (a -> a1) -> a -> a -> Bool
iguales f g n m
   | (n > m) = False
   | (n == m) && (f n == g n) = True
   | (f n == g n) = iguales f g (succ n) m
   | otherwise = False

cuantos:: (Integral a) =>(a->Bool)->[a]->a
cuantos p [] = 0
cuantos p (x:xs)
   | p x = 1+cuantos p xs
   | otherwise = cuantos p xs
