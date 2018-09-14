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