millionYearsToSec x = x*1000000*365*24*60*60*1000/1

tupleSecsTo x = (secToYears x, remainingDays x, remainingHours x, remainingMin x, remainingSecs x)

secToYears x = div (div (div (div x 60) 60) 24) 365

remainingDays x = (div(div(div x 60) 60) 24) `mod` 365

remainingHours x = (div(div x 60) 60) `mod` 24

remainingMin x = (div x 60) `mod` 60 

remainingSecs x = x `mod` 60

--ejer 2
f x y = 2*x-y*x
g x = f(f 2 x) (f x 1)
h x y z = f((f(x+2y) g 3) (5 - g z - y))
i x y = if x >= y && y > 0 then x - y
		else if 0 < x && x < y then 0
		else y - x