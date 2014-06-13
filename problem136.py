#!/usr/bin/python

from time import time
from sys import stdout

def main():
	n = 10000000
	factors = factor_sieve(n)
#	print factors

def factor_sieve(n):
	factors = [True] * n
	factors[0:2] = [False, False]
	for m in range(2,n//2+1):
		if factors[m]:
			for i in range(2 * m, n, m):
				factors[i] = False
				stdout.write("%d: %d\r" %(m, i))
				stdout.flush()
	return factors

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
