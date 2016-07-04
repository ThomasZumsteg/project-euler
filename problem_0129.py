#!/usr/bin/python
"""http://projecteuler.net/problem=129"""

from time import time
from itertools import count
from sys import stdout
from common import prime_sieve

def main():
	big_num = 1000000
	for n in count(big_num + 1, 2):
		if n % 5 == 0: continue
		a = A(n)
		if a > big_num:
			print "%d: %d" %(n,a)
			return
		stdout.write("%d: %d\r" %(n,a))
		stdout.flush()

def factor_sieve(lim):
	sieve = [set() for _ in range(lim)]
	for i in range(2,lim):
		if not sieve[i]:
			for j in range(2*i, lim, i):
				sieve[j].add(i)
	return sieve

def A(n):
	rep = 1
	for i in count(1):
		rep %= n 
		if rep == 0:
			return i
		rep = rep * 10 +1 

if __name__ == "__main__":
	start = time()
	main()
	#factor_sieve(1000000)
	print "That took %f seconds" %(time() - start)