fib :: Integer -> Integer
fib n 
	| n == 0 = 0
	| n == 1 = 1
	| otherwise = fib(n-1) + fib(n-2)

factorial :: Integer -> Integer
factorial n
	| n > 0 = n * factorial (n-1)
	| n == 0 = 1

sucesion1 :: Integer -> Integer
sucesion1 n 
	| n == 1 = 2
	| otherwise = 2 * (n-1) * sucesion1(n-1) + 2 ^ n * factorial(n-1)

sucesion2 :: Integer -> Integer
sucesion2 n
	| n == 1 = 1
	| n == 2 = 2
	| otherwise = (n-2) * sucesion2(n-1) + 2 * (n - 1) * sucesion2(n-2)

sucesion3 :: Integer -> Integer
sucesion3 n
	| n == 1 = -1
	| n == 2 = 6
	| mod n 2 /= 0 = -1 * sucesion3(n-1) - 3
	| mod n 2 == 0 = sucesion3(n-3) + 2 * sucesion3(n-2) + 9