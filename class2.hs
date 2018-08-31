f n | n == 0 = 1
	| n > 4 = 2
	| otherwise = 1

signo n | n > 0 = 1
		| n == 0 = 0
		| n < 0 = -1

absoluto n | n == 0 = 0
		   | n > 0 = n
		   | n < 0 = n * (-1)

maximo a b | a < b = b
		   | a > b = a
		   | otherwise = a

maximo3 a b c = maximo(maximo a b) c

esPar :: Integer -> Bool
esPar x | mod x 2 == 0 = True
		| otherwise = False

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | mod x y == 0 = True
				 | otherwise = False