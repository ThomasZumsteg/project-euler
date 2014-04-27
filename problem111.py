#!/usr/bin/python
"""http://projecteuler.net/problem=111"""

from time import time
from common import prime_sieve
from itertools import combinations

def main():
	l = 10
	s = 0
	for d in [str(x) for x in range(10)]:
		s += prime_run(d, l)
	print "PE111: %d" %(s)

def prime_run(d, length):
	N = 0
	S = 0
	for M in reversed(range(1,length+1)):
#		print M
		for mask in mask_gen(length,M):
			if d == "0" and mask[0]: continue
#			print mask
			for fill in fill_gen(d,length-M):
				if fill[0] == '0' and not mask[0]:
					continue
				p = zip(mask, fill, d)
				if is_prime(p) and len(str(p))==length:
					N += 1
					S += p
		if N != 0:
			print "%s: %d: %3d: %d" %(d,M,N,S)
			return S
	# No matches (never going to happen)

def is_prime(n):
	if n<2:
		return False
	if n in (2,3):
		return True
	if n % 2 == 0 or n % 3 == 0:
		return False
	max_divisor = int(n**0.5)+1
	divisor = 5
	while divisor <= max_divisor:
		if n % divisor == 0 or n % (divisor + 2) == 0:
			return False
		divisor += 6
	return True

def zip(mask, fill, d):
	p = ['-'] * len(mask)
	j = 0
	for i,v in enumerate(mask):
		if v:
			p[i] = d
		else:
			p[i] = fill[j]
			j += 1
	return int(''.join(p))

def fill_gen(d,l):
	p_string = "%0" + str(l) + "d"
	for n in range(int(10**l)):
		array_n = tuple(p_string % n)
		if d not in array_n:
			yield array_n
#		else: print "Skipped %s" %(array_n)

def mask_gen(l,n):
	for group in combinations(range(l),n):
		yield [True if i in group else False for i in range(l)]

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
