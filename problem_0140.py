#!/usr/bin/python
"""Find the fifteenth rational input for the sequence Ag(x)=n, for n = 1,2,3,4...
Ag(x) = x * G1 + x^2 * G2 + x^3 * G3 + ... + x^k * Gk
where Gk = Gk-1 + Gk-2, G1 = 1 and G2 = 4
Details: http://projecteuler.net/problem=140"""

from time import time

def main():
	"""Finds the sum of the first 30 golden nuggets. A golden nugget statisfis Ag(x) = n for n = 1,2,3,4,5... and x is rational"""
	# Relevent diophantine base cases
	# from http://www.alpertron.com.ar/QUAD.HTM
	queue = [diphan_gen(7,1),
			 diphan_gen(8,2),
			 diphan_gen(13,5),
			 diphan_gen(17,7),
			 diphan_gen(32,14),
			 diphan_gen(43,19),
			]
	i = 0
	s = 0
	while i <= 30:
		(n,_) = queue[0].next()
		queue.append(queue.pop(0))
		if (n-7) % 5 == 0:
			s += (n - 7) / 5
			i += 1
	return s

def diphan_gen(x,y):
	"""Generates a diophantine sequence for a base case (x,y) for problem140"""
	# From http://www.alpertron.com.ar/QUAD.HTM
	# x^2 - 5 * y^2 - 44 = 0
	(p,q,r,s) = (9,20,4,9)
	while True:
		yield (x,y)
		(x,y) = (p * x + q * y, r * x + s * y)

if __name__ == "__main__":
	start = time()
	print "Answer: %d" % main()
	print "That took %f seconds" %(time() - start)
