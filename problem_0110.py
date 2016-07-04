#!/usr/bin/python
"""http://projecteuler.net/problem=110"""

from time import time
from math import log
from common import prime_sieve

def main():
	N = 4000000
	primes = prime_sieve(100)
	m = int(log(2*N,3)) + 1
	factors = [1] * m
	best = (quick_num(factors, primes), factors)
	done = [factors]
	for i in reversed(range(m)):
		queue = done
		done = set()
		for f in queue:
			for new in replace(f[:], primes, i, N):
				done.add(new)
				s = quick_num(new, primes)
				best = min(best, (s, new))
	print "%d: %d" %(number(best[1], primes), solutions(best[1]))

def replace(f, p, i, N):
	f = list(f)
	f[i] -= 1
	for n in range(2,p[i]):
		new = join(f, make_from_parts(n,p))
		if ordered(new) and has_enough_sols(new,N):
			yield tuple(new)

def ordered(l):
	for i in range(1,len(l)):
		if l[i-1] < l[i]:
			return False
	return True

def make_from_parts(n,p):
	new = [0] * len(p)
	i = 0
	while n > 1:
		if n % p[i] == 0:
			n /= p[i]
			new[i] += 1
		else: i += 1
	return new

def join(a,b):
	c = a[:]
	for i,t in enumerate(b):
		try: c[i] += t
		except IndexError:
			c.append(t)
	return c

def solutions(factors):
	sols = 1
	for f in factors:
		sols *= (1+2*f)
	return (sols + 1) / 2

def number(factors, primes):
	num = 1
	for f,p in zip(factors, primes):
		num *= p**f
	return num

def quick_num(factors, primes):
	# Keeps number as float
	# For comparisons only
	num = 0
	for f,p in zip(factors, primes):
		num += f * log(p)
	return num

def has_enough_sols(factors,N):
	# Has more then N unique solutions
	sols = 0
	for f in factors:
		sols += log(1 + 2*f)
	return sols >= log(2*N)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
