#!/usr/bin/python
"""Finds the smallest integers x + y + z where x > y > z > 0 such that x+y, x-y, x+z, x-z, y+z, and y-z are all perfect squares
Details: http://projecteuler.net/problem=142"""

from time import time
from sys import stdout
from itertools import count, combinations

def main():
	"""Steps though centers with two or more gaps (possiable x's) to determine if those gaps are a center and gap pair, y and z respectivly"""
	#    |---y---|-z-|                  |-z-|          Gaps
	#    <---<---x--->--->          <---y--->          Centers
	# <----------|----------------------|------------0 Number line
	for (centers, x) in center_and_gap_gen():
		for (z,y) in combinations(sorted(centers[x]),2):
			if (y in centers) and (z in centers[y]):
				return sum((x,y,z))
def center_and_gap_gen():
	"""Finds the center and gap for prime pairs by stepping through pairs of prime
	yields the center dictionary and center when a center with two different gaps are found"""
	centers = {}
	for n in count(1):
		for m in range(1,n):
			# Center of two square pairs (n+m)^2 and (n-m)^2
			# ((n+m)^2 + (n-m)^2) / 2 = center
			center = n**2 + m**2
			# Difference between the square and the center
			# (center - (n-m)^) / 2 == ((n+m)^2 - center)^2 / 2 = diff
			diff   = 2 * n * m
			append(centers, center, diff  )
			if len(centers[center]) >= 2:
				yield (centers, center)

def append(dictionary, key, value):
	"""Adds value to dictionary key even if it doesn't exist"""
	try: dictionary[key].add(value)
	except KeyError: dictionary[key] = set([value])

def brute():
	""""Brute forces the problem by stepping through x,y, and z. Really slow"""
	for x in count(3):
		for y in range(2,x):
			for z in range(1,y):
				stdout.write("(%5d, %5d, %5d)\r" %(x,y,z))
				stdout.flush()
				if all_square(x,y,z):
					print "(%5d, %5d, %5d): %6d" %(x,y,z, x + y + z)

def all_square(x,y,z):
	"""Checks if all combinations of x>y>z>0 are square"""
	for n in [x+y, x-y, x+z, x-z, y+z, y-z]:
		if not is_square(n): return False
	return True

def is_square(N):
	"""Checks if N is a perfect square, e.g. 4,9,16,25..."""
	n = int(N**0.5)
	return n**2 == N

if __name__ == "__main__":
	"""Scriptify and time main"""
	start = time()
	print "Answer: %d" %( main())
	print "That took %f seconds" %(time() - start)