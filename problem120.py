#!/usr/bin/python
"""http://projecteuler.net/problem=120"""

from time import time
from sys import stdout
from itertools import count

def main():
	r_max_sum = 0
	for n in range(3,1001):
		stdout.write("%d\r" %(n))
		stdout.flush()
		r_max = 0
		for r in square_remainer_gen(n):
			r_max = max(r,r_max)
		r_max_sum += r_max
	print "The remainer sum is %d" %(r_max_sum)
	

def square_remainer_gen(n):
	(  i,   j) = (n+1, n-1)
	(r_i, r_j) = (  i,   j)
	n_mod      = n**2
	while (r_i != 1) or (r_j != 1):
		yield (r_i + r_j) % n_mod
		r_i = (r_i*i) % n_mod
		r_j = (r_j*j) % n_mod
	yield (r_i + r_j) % n_mod

def slow_gen(n):
	i = n+1
	j = n-1
	p = 1
	while True:
		yield (i**p + j**p) % n**2
		p += 1

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() -start)
