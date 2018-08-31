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


crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

invertir :: (a, b) -> (b, a)
invertir p = (snd p , fst p)

normaVectorial  :: (Float , Float) -> Float
normaVectorial p = sqrt ((fst p) ^ 2 + (snd p) ^ 2)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos a b = sqrt( ( (fst a) - (fst b) )^2 + ((snd a) - (snd b))^2 )

restaPuntos :: (Float, Float) -> (Float,Float) -> (Float, Float) 
restaPuntos a b = ( fst a - fst b , snd a - snd b )

distanciaPuntos2 :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos2 a b = normaVectorial(restaPuntos a b) 