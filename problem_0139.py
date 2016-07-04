#!/usr/bin/python
"""Finds the number of integer right trangles with perimeter less then one-hundered million that can be tiled to create an square integer area
Further explination: http://projecteuler.net/problem=139"""

from time import time
from itertools import count

def main():
	# Wrapper for number_of_solutions
	big_num = 100000000
	return number_of_solutions(big_num)

def number_of_solutions(lim):
	"""Find the number of solutions to the pythagorean tile problem by taking a generated base case and determining the number of multiples less then [lim]"""
	cases = 0
	for base in case_gen():
		#FIX_ME : Find pattern in base cases sum
		s = sum(base)
		if s >= lim: break
		else: cases += lim / s
	return cases

def case_gen():
	"""Generates base cases for the pythagorean tile problem by generating pythaogrean triples for which c % abs(a-b) == 0"""
	# See http://en.wikipedia.org/wiki/Pythagorean_triplets#Generating_a_triple
	for m in count(2):
		# required conditions for uniquness m > n, m - n is odd, and m & n are coprime (gcd == 1)
		for n in range(1 if m % 2 == 0 else 2, m, 2):
			if gcd(m,n) == 1:
				a = m**2 - n**2
				b = 2 * m * n
				c = m**2 + n**2
				if c % abs(b - a) == 0:
					yield (a,b,c)

def gcd(a,b):
	"""Finds the greatest common factor using Euclid's algorithm. See http://en.wikipedia.org/wiki/Euclid%27s_algorithm"""
	while b != 0: (a,b) = (b, a % b)
	return a

def brute(lim):
	"""Generates pythagorean triplets and checks for tileability"""
	# Pythgorean triplets formula from http://en.wikipedia.org/wiki/Pythagorean_triples#Generating_a_triple
	# Limit for m is based on the equation 2 * m**2 + 2*m - 1 < lim
	L = int(((2 * lim + 3)**0.5 - 1) // 2 + 1)
	# Set checks triangle uniquness
	l = set()
	for m in range(2, int((lim/2 + 1)**0.5 - 1)):
		for n in range(1 if m % 2 == 0 else 2, m, 2):
			if gcd(m,n) == 1:
				a = m**2 - n**2
				b = 2 * m * n
				c = m**2 + n**2
				if c % abs(b - a) == 0:
					# multiples of the base case
					for k in range(1, lim/sum((a,b,c)) + 1):
						l.add(tuple(sorted((k * a, k * b, k * c))))
	return len(l)

if __name__ == "__main__":
	start = time()
	print "The answer is %d" % main()
	print "That took %f second" %(time() - start)