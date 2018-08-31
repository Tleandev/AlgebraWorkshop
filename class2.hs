f n | n == 0 = 1
	| n > 4 = 2
	| otherwise = 1

signo n | n > 0 = 1
		| n == 0 = 0
		| n < 0 = -1

absoluto n | n == 0 = 0
		   | n > 0 = n
		   | n < 0 = n * (-1)