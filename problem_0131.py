#!/usr/bin/python
"""http://projecteuler.net/problem=131"""

from time import time
from common import prime_generator
from itertools import count
from sys import stdout

def main():
	lim = 1000000
	ans = 0
	for p in prime_generator(block = lim + 1000):
		if p >= lim: break
		(m, args) = match(p)
		if m:
			(a,c) = args
			stdout.write("%d^3 + %d^2 * %d = %d^3\r" %(a,a,p,c))
			stdout.flush()
			ans += 1
	print "There are %d primes with propery below %d" %(ans, lim),
	print " " * 10

def brute():
	lim = 100000
	for p in prime_generator():
		if p >= lim: break
		for n in range(1,lim):
			m = n**2 * (n + p)
			if is_cube(m):
				if is_cube(n):
					nu = round(n ** (1.0/3))
				print "%d^9 + %d^2 * %d = %d^3" %(nu,n,p,round(m**(1.0/3)))

def match(p):
	lim = (p - 3.0 / 4)**(0.5) - 0.5
	for a in range(1,int(lim) + 1):
		b = a**3 + p
		if is_cube(b):
			c = int(b ** (1.0 / 3) * a ** 2)
			return (True, (a ** 3, c))
	return (False, None)

def is_cube(n):
	rt = round(n ** (1.0/3))
	return rt ** 3 == n

def factor_sieve(lim):
	sieve = [[] for _ in range(lim)]
	for i in range(2,lim):
		if not sieve[i]:
			for j in range(i,lim,i):
				tmp = j
				while tmp % i == 0:
					sieve[j].append(i)
					tmp /= i
	return sieve

if __name__ == "__main__":
	start = time()
	main()
	#factor_sieve(1000000)
	print "That took %f seconds" %(time() - start)