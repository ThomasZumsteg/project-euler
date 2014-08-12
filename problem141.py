#!/usr/bin/python
"""Find the sum of all such n's less then one trillion

Given n,d,q and r such that:
	1) q and r are the quotient and remainer of n divided by d
	2) n, d and r are consecutive positive integers in a geometric sequence, but not necessarialy in that order.
	3) n is a perfect square

Details: http://projecteuler.net/problem=141"""

from time import time
from itertools import count
from sys import stdout

def main():
	"""Wrapper for timing quick_brute"""
	s = set()
	for case in quick_brute(10**12):
		# Checks for redundant cases
		s.add(case[0])
	return sum(s)

def quick_brute(lim):
	"""Brute forces solutions to problem.
	Based on the equation n = q * b + r where q > b > r,
	Common ratio between q, b, and r is x/y where x > y,
	r must be divisable by y^2 and can be represented r = z * y**2 for z >= 1"""
	for x in range(2,int(lim**(1.0/3))+1):
		for y in range(1,x):
			# Skip if not a reduced fraction
			if gcd(x,y) != 1: continue
			for z in count(1):
				# Working equation is N = x**3 * y**2 * z + y**2 * z
				N = z**2 * x**3 * y + z * y**2
				if N > lim: break
				if is_square(N):
					yield N

def is_square(n):
	"""Does what it says on the tin"""
	m = int(n**0.5)
	return m**2 == n

def gcd(a,b):
	"""Finds the greatest common factor using Euclid's algorithm. See http://en.wikipedia.org/wiki/Euclid%27s_algorithm"""
	while b != 0: (a,b) = (b, a % b)
	return a

def brute(lim):
	"""First attempt, cycles through n and d, slow"""
	# Sample output and solutions
	#		  n =     q *     d +     r: (    c)
	#         9 =     4 *     2 +     1: ( 2/ 1)
	#     10404 =   144 *    72 +    36: ( 2/ 1)
	#     16900 =   225 *    75 +    25: ( 3/ 1)
	#     97344 =  1058 *    92 +     8: (23/ 2)
	#    576081 =  1600 *   360 +    81: (40/ 9)
	#   6230016 =  4232 *  1472 +   512: (23/ 8)
	#   7322436 =  3025 *  2420 +  1936: ( 5/ 4)
	#  12006225 =  4900 *  2450 +  1225: ( 2/ 1)
	#  36869184 =  6400 *  5760 +  5184: (10/ 9)
	#  37344321 =  7056 *  5292 +  3969: ( 4/ 3)
	#  70963776 =  9522 *  7452 +  5832: (23/18)
	# 196112016 = 44652 *  4392 +   432: (61/ 6)
	# 256160025 = 39204 *  6534 +  1089: ( 6/ 1)
	#1361388609 = 44100 * 30870 + 21609: (10/ 7)
	#1380568336 = 53361 * 25872 + 12544: (33/16)
	for n in range(1,int(lim**0.5)):
		N = n**2
		stdout.write("%10d\r" %(N))
		stdout.flush()
		for d in range(2,n):
			r = N  % d
			q = N // d
			if r <= 0: continue
			if (d ** 2 / float(r)) == q:
				g = gcd(d,r)
				case = (N, n, q, d, r, d/g, r/g)
				yield case
				break

if __name__ == "__main__":
	"""Timing and scriptifying"""
	start = time()
	print "Answer: %d" %( main() )
	print "That took %f seconds" %(time() - start)