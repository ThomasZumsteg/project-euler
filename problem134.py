#!/usr/bin/python
"""http://projecteuler.net/problem=134"""

from time import time
from sys import stdout
from common import prime_generator, is_prime
from itertools import count

def main():
	big_num = 1000000
	p_big = 1
	s_sum = 0
	for (large, small) in prime_pairs(big_num):
		if large == 5: continue
		stdout.write("%d: %d\r" %(large, small))
		stdout.flush()
		s_sum += get_s(small, large)

def prime_pairs(lim, block = 1000100):
	pg = prime_generator(block)
	large = pg.next()
	for p in pg:
		(large, small) = (p, large)
		if small > lim: return
		if large - small == 2:
			yield (large, small)
			
def get_s(p1, p2):
	ord = get_ord(p1)
	(m, n) = diophantine(p2, -ord, p1)
	return n

def diophantine(a, b, c):

	

def get_ord(n):
	for i in count(1):
		n = n // 10
		if n == 0: return 10 ** i

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)