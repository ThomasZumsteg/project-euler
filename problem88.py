#!/usr/bin/python
"""http://projecteuler.net/problem=88"""

from time import time
from sys import stdout

def main():
	factor_sets = {}
	sieve = new_factor_sieve(20000)
	print "Starting now"
	k = [False] * (12000 - 2)
	pnmins = set()
	n = 0
	while False in k:
		n += 1
		for f in sieve[n]:
			pnnum = n - sum(f) + len(f)
			if pnnum-1 <= len(k) and not k[pnnum-2]:
				k[pnnum-2] = True
				pnmins.add(n)
				stdout.write("%d remaining\r" %(k.count(False)))
				stdout.flush()
	print sum(pnmins) + " " * 5

def product_set_gen(num, factors):
	for f in factors:
		remainer = num
		product_set = []
		while remainer % f == 0 and remainer//f != 1:
			remainer /= f
			product_set += [f]
			yield product_set + [remainer]

def factor_sieve(N):
	sieve = [set() for x in range(N)]
	for a in range(2,N):
		if sieve[a]: 
			add_multiples(sieve,a)
		else:
			for n in range(2*a,N,a):
				b = n//a
				l = sorted((a,b))
				sieve[n].add(tuple(l))
	return sieve

def new_factor_sieve(N):
	sieve = [set() for x in range(N)]
	for n in range(2,N):
		for a in range(2,n):
			if n % a != 0: continue
			b = n // a
			sieve[n].add(tuple(sorted((a,b))))
			for f in sieve[b]:
				l = sorted(list(f) + [a])
				sieve[n].add(tuple(l))
	return sieve

def add_multiples(sieve, num):
	for m,n in [x for x in sieve[num]]:
		for m_set in sieve[m]:
			l = sorted([n] + list(m_set))
			sieve[num].add(tuple(l))
		for n_set in sieve[n]:
			l = sorted([m] + list(n_set))
			sieve[num].add(tuple(l))

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start )
