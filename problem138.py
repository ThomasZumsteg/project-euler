#!/usr/bin/python
"""Finds the sum of twelve smallest identical lengths of an isosceles triangle where the base is one less or greater then the hight, and the base and length are integers
Further explination: http://projecteuler.net/problem=138"""

from time import time
from itertools import count
from sys import stdout

def main():
	# finds the sum of the first 12 values lengths
	lim = 12
	length_sum = 0
	for sum_count, length in enumerate(fast_gen(),1):
		length_sum += length
		if sum_count >= lim:
			return length_sum

def fast_gen():
	"""Wrapper for pell_gen"""
	queue = [pell_gen(1,4)]
	while queue:
		yield queue[0].next()[0]
		queue.append(queue.pop(0))

def pell_gen(x,y):
	"""Finds integer solutions to 20x^2 - y**2 - 4 = 0, given base x,y"""
	# coefficents found using http://www.alpertron.com.ar/QUAD.HTM
	# Does not print the base case
	while True:
		(x,y) = (9 * x + 2 * y, 40 * x + 9 * y)
		yield (x,y)

def brute():
	"""Solves the problem slowly by iterating though L"""
	for length in count(2):
		# Run status, size of length
		stdout.write("%d\r" %(length))
		stdout.flush()

		# equation is 20 * length^2 - 4 = (5 * base +- 4)^2 = L = B^2
		# L and B are temp variables
		L = (20 * (length ** 2)) - 4
		if is_square(L):
			B = L ** 0.5
			# Case: height = base + 1
			if (B - 4) % 5 == 0:
				base = (B - 4) / 5
				height = base + 1
				print "%d: %d: %d" %(length, base, height)
			# Case: height = base - 1
			elif (B + 4) % 5 == 0:
				base = (B + 4) / 5
				height = base - 1
				print "%d: %d: %d" %(length, base, height)

def is_square(N):
	""""Does what it says on the tin"""
	n = int(N**0.5)
	return n**2 == N

if __name__ == "__main__":
	"""wrapper for main"""
	start = time()
	print "The answer is %d" % main()
	print "That took %f seconds" %(time() - start)