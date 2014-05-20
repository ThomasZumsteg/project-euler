#!/usr/bin/python
"""http://projecteuler.net/problem=133"""

from time import time
from itertools import count
from common import prime_generator
from sys import stdout

def main():
	big_num = 100000
	prime_sum = 2 + 3 + 5
	for p in prime_generator(block=100100):
		if p <= 5: continue
		if p >= big_num: break
		stdout.write("%d\r" %(p))
		stdout.flush()
		if not match(p):
			prime_sum += p
	print "Sum of all primes with property is %d" %(prime_sum)

def has_other_factors(n, factors):
	for f in factors:
		while n % f == 0:
			n /= f
	return n != 1

def match(n):
	return not has_other_factors(A(n), (2, 5))

def has_match_old(n):
	seen = set()
	for i in count(1):
		rem = pow(10, 10**i, 9*n)
		if rem == 1: return True
		elif rem in seen: return False
		#print rem
		seen.add(rem)
		#if i > 20: break

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
	print "That took %f seconds" %(time() - start)