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
--f x y = (-)((*) 2 x) ((*) y x)
--g x = f(f 2 x) (f x 1)
--h x y z = f((f((+) x 2y) g 3) ((-)((-) 5 (g z)) y))
--i x y = if x >= y && y > 0 then (-) x y
  --else if 0 < x && x < y then 0
  --else (-) y x



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
last_2 (x:xs) 
  | xs == [] = x
  | otherwise = last_2 xs

init_2 (x:xs) 
  | xs == [] = []
  |otherwise = x:init_2 xs

initLast (x:xs) = (init_2 (x:xs), last_2 (x:xs))

concat_l [] = []
concat_l (x:xs) = x++concat_l xs

take_n n [] = []
take_n 0 _ = []
take_n n (x:xs) = x: take_n (n-1) xs

drop_n n [] = []
drop_n 1 (x:xs) = xs
drop_n n (x:xs) = drop_n (n-1) xs

splitAt_n n (x:xs) = (take_n n (x:xs),drop_n n (x:xs))

reverse_2 [] = []
reverse_2 (x:xs) = (reverse_2 xs)++[x]


--Exer 8 
--La primera forma esta en el ejer anterior

reverse_3 [] [] = []
reverse_3 ys [] = ys
reverse_3 ys (x:xs) = reverse_3 (x:ys) xs

--Exer 10 
--divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]

digits n
  | (n `div` 10 == 0) = [n]
  | otherwise = (digits (n `div` 10))++[n `mod` 10]

n_digits n
  | (n `div` 10 == 0) = 1
  | otherwise = 1+n_digits (n `div` 10)

pair n = (all (even) (digits n))

------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------HOJA 2-----------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------
--Exer 1.1

sq_50 :: (Num t, Eq t) => t -> [t]
sq_50 51 = []
sq_50 x = x^2:sq_50(x+1)

sq_50_l x = take (x+1) (map (^2) [0..])


--Exer 1.2
sq_50_2 51 = []
sq_50_2 x = (x,x^2):sq_50_2(x+1)

sq_50_l_2 x = zip [0..] (sq_50_l x)

--exer 1.3
pow_3 x
   | x*3 >= (10^10) = []
   | otherwise = (x*3):pow_3 (x*3)

--Exer 1.4
fst_pow_3 y
   | y >= 1000 = []
   | otherwise = (3^y):fst_pow_3(y+1)

fst_pow_3_2 = take 1000 (iterate (*3) 1)

fst_pow_3_3 = take 1000 (map (3^) [1..])

--Exer 1.5
sum_3_5 = sum (map mul_3_5 [1..1000])

mul_3_5 x
   | (x `mod` 3 == 0) = x
   | (x `mod` 5 == 0) = x
   | otherwise = 0

sum_3_5_v2 = sum (filter bool_mul_3_5 [1..1000])

bool_mul_3_5 x = (x `mod` 3 == 0) || (x `mod` 5 == 0)

--Exer 1.6

--Exer 1.7
list_pow x
  | x >= 10 = []
  | otherwise = (map(^x)([1..20])):list_pow(x+1)

--Exer 1.8
list_pow_2 x
  | x >= 10 = []
  | otherwise = (map(x^)([1..20])):list_pow_2(x+1)

--Exer 1.9
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]

primo x = (length (divisors x)) == 1

lower_1000_prime = filter (primo) [1..1000]

--Exer 1.10
media_1000 = (sum lower_1000_prime) `div` (length lower_1000_prime)

--Exer 1.11
num_prim_entre_200_500 = length(filter (primo) [200..500])

--Exer 1.12
first_prime_6923 (x:xs)
   | (x > 6923) && (primo x) = x
   |otherwise = first_prime_6923 xs

--Exer 1.13
num_and_div x
   | x <= 50 = (x, divisors x):num_and_div(x+1)
   |otherwise = []

--Exer 1.14
perfecto x = x == sum (divisors x)

perfect_list x = filter (perfecto) [1..x]

--Exer 1.15
prime_not_next_30 x = (primo x) && (length (filter (primo) [x+1..x+30]) == 0)

first_sep_prime (x:xs)
   | prime_not_next_30 x = x
   |otherwise = first_sep_prime xs

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

{-mayoria (a->Bool)->[a]->Bool
mayoria p [] = 
mayoria p (x:xs) 
   | p x-}

menorA :: (Num a, Ord a) => a -> a -> (a -> Bool) -> a
menorA n m p
   | (n > m) = m+1
   | (p n) = n
   | otherwise = (menorA (n+1) m p)

menor :: Num a => a -> (a -> Bool) -> a
menor n p
   | (p n) = n
   | otherwise = (menor (n+1) p)

mayorA :: (Num a, Ord a) => a -> a -> (a -> Bool) -> a
mayorA n m p
   | (n > m) = n-1
   | (p m) = m
   | otherwise = (mayorA n (m-1) p)

mayor :: Num a => a -> (a -> Bool) -> a
mayor n p
   | (p n) = n
   | otherwise = (mayor (n-1) p)

ex :: (Num a, Ord a) => a -> a -> (a -> Bool) -> Bool
ex n m p
   | (n > m) = False
   | p n = True
   | otherwise = ex (n+1) m p

pt :: (Num t, Ord t) => t -> t -> (t -> Bool) -> [t]
pt n m p
   | (n > m) = []
   | p n = n:pt (n+1) m p
   | otherwise = pt (n+1) m p


--Exer 3
f n
  | n `mod` 2 == 0 = n `div`2
  |otherwise = 3*n + 1

f' n = (length(takeWhile (>1) (iterate f n)) +1 , takeWhile (>1) (iterate f n))

num_pas n = length(takeWhile (>1) (iterate f n)) +1

f'' n = zip [1..n] (map num_pas [1..n])

--Exer 5
g x y = let z = f_2 x y in (z,x*y,z+1)
g' x y = (f_2 x y, x*y, f_2 x y + 1)
g'' x y = h x y (f_2 x y)
h x y z = (z,x*y,z+1)
--f_2 x y = 2*x + y


--apartado 2
f_2 x y = ((2*x + y)^x * y)* (x*y+y^2) -- la mas lenta es la segunda porque tiene dos llamadas a f_2 entonces si esa funcion fuera costosa seria la unica que la computaria dos veces


--apartado 3
f1 x y = let z = x*(y+1) in (z,x*z,z+y)
f1_2 x y = (aux_f1 x y, x * aux_f1 x y,aux_f1 x y + y)
aux_f1 x y = x*(y + 1)


f2 x y = let h z = z + x in (h 3,h x, h y)
f2_2 x y = (aux_f2 x 3, aux_f2 x x, aux_f2 x y)
aux_f2 x z = z + x