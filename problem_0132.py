#!/usr/bin/python
"""http://projecteuler.net/problem=132"""

from time import time
from common import prime_generator, is_prime
from itertools import count
from sys import stdout

def main():
	big_num = 10**9
	lim = 40
	c = 0
	prime_sum = 0
	for p in prime_generator(block=10000000):
		if p == 5 or p == 2: continue
		a = A(p)
		stdout.write("%d: %d\r" %(c,p))
		stdout.flush()
		if big_num % a == 0:
			#print p
			prime_sum += p
			c += 1
			print "%d: %d" %(c, p)
		if c >= lim: break
	print "Sum of the first %d primes is %d" %(lim, prime_sum)

def factor(n):
	f = []
	p_gen = prime_generator()
	while not is_prime(n) and n > 1:
		p = p_gen.next()
		while n % p == 0:
			n /= p
			f.append(p)
			print n
	if n != 1: f.append(int(n))
	return f

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