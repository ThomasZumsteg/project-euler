#!/usr/bin/python
"""Solved http://projecteuler.net/problem=137"""

from time import time
from itertools import count
from sys import stdout

# 1:         2
# 2:        15
# 3:       104
# 4:       714
# 5:      4895
# 6:     33552
# 7:    229970
# 8:   1576239
# 9:  10803704
#10:  74049690

def main():
	for i, nugget in enumerate(gold_nugget_gen(),1):
		stdout.write( "%d: %d\r" %(i, nugget))
		stdout.flush()
		if i == 15:
			print "%d: %d" %(i, nugget)
			break

def gold_nugget_gen():
	# see http://www.alpertron.com.ar/QUAD.HTM for base cases
	# Every other solution works, not sure why
	queue = [pell_gen(-1,1), pell_gen(1,1), pell_gen(4,2)]
	while True:
		yield queue[0].next()
		queue.append(queue.pop(0))
		queue[0].next()
		queue.append(queue.pop(0))

def pell_gen(x,y):
	# See http://www.alpertron.com.ar/QUAD.HTM for coefficients
	while True:
		(x,y) = (9*x+20*y, 4*x+ 9*y)
		yield (x-1)/5

def continued_fration(N):
	# continued fraction expansion of N**0.5
	# Not used
	(A_0, A_1) = (1, int(N**0.5))
	(B_0, B_1) = (0, 1)
	while True:
		yield (A_1, B_1)
		#(A_0, A_1) = ( ,A_0)
		#(B_0, B_1) = ( ,B_0)
		
def brute():
	# Slow but works
	nuggets = 1
	for n in count(1):
		stdout.write("%2d: %9d\r" %(nuggets, n))
		stdout.flush()
		if is_square(5*n**2 + 2*n + 1):
			print "%2d: %9d" %(nuggets, n)
			if nuggets >= 15: return
			nuggets += 1

def is_square(N):
	return N == (int(N**0.5))**2

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
