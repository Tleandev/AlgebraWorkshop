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

f1 :: Integer -> Integer
f1 n 
    | n == 1 = 1
    | n > 1 = 2 ^ n + f1(n-1)

f2 :: Integer -> Float -> Float
f2 n q
    | n == 1 = q
    | otherwise = q ^ n + f2 (n-1) q

f3 :: Integer -> Float -> Float
f3 n q
    | n == 1 = q
    | otherwise = q ^ (2*n) + f3 (n-1) q

f4 :: Integer -> Float -> Float
f4 n q
    | n == 0 = 1
    | otherwise = f4 (n-1) q + q ^ (2*n-1) + q ^ (2*n) - q ^ (n-1)

esPar :: Integer -> Bool
esPar n 
    | n == 0 = True
    | n == 1 = False
    | otherwise = esPar (n-2)

divide3 :: Integer -> Bool
divide3 n
    | n == 0 = True
    | n == 1 = False
    | n == 2 = False
    | otherwise = divide3 (n-3)