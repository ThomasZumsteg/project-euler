#!/usr/bin/python
"""http://projecteuler.net/problem=128"""

from time import time
from itertools import count
from sys import stdout

def main():
	count = 2
	big_num = 2000
	for num, diffs in diff_gen():
		#print "%d: %s" %(num, diffs),
		primes = map(is_prime, diffs)
		if primes.count(True) == 3:
			count += 1
			stdout.write("%3d: %8d: %s\r" %(count, num, diffs))
			stdout.flush()
		if count >= big_num: break
	print "The %d tile in the sequence is %d" %(count, num) + " " * 30

def diff_gen():
	num = 8
	for n in count(2):
		for m in range(6):
			yield (num, corner_diffs(n,m))
			num += n
		yield (num - 1, end_diff(n))

def corner_diffs(n,m):
	(down, up) = (6 * (n - 1) + m, 6 * n + m)
	if m != 0:
		return (1, up + 1, up, up - 1, 1, down)
	else:
		large_back = 12 * n + 5
		return (down, 1, up + 1, up, large_back, up - 1)

def end_diff(n):
	if n == 1: return (1, 6, 5, 12, 11, 10)
	up = 6 * ( n + 1)
	down = 6 * n
	double_down = 12 * n - 7
	return (down, double_down, down - 1, up, up - 1, 1)

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

def brute_test():
	upper = (2,3,4,5,6,7)
	current = (1)
	lower = None
	for i in count(1):
		(lower, current) = (current, upper)
		upper = tuple(range(current[-1] + 1, current[-1] + 6 * i + 7))
		rings = (upper, current, lower)
		#print "%s\n%s\n%s" %(rings[0],rings[1], rings[2])
		#if i > 3: break
		for n, diffs in zip(current, make_diffs(rings)):
			print "%3d: %s" %(n, diffs)

def make_diffs(rings):
	r = len(rings[1]) // 6
	edge   = edge_gen(r)
	corner = corner_gen(r)
	for n in range(r * 6):
		if n % r == 0:
			# Corner
			diff_map = corner.next()
		else:
			# Edge
			diff_map = edge.next()
		yield map_diffs(diff_map, rings, n)

def edge_gen(r):
	#WIP
	yield ((1,i-1),(1,i+1),(0,0),(0,1),(2,1),(2,2))
	yield ((1,i-1),(1,i+1),(0,1),(0,2),(2,4),(2,5))

def corner_gen(r):
	for i in range(6):
		yield ( (1, i * r - 1),
		        (1, i * r + 1),
				(0, i * (r - 1)),
				(2, i * (r + 1) - 1),
				(2, i * (r + 1)),
				(2, i * (r + 1) + 1) )

def gen_neighbours(n):
	if (n - 2) % 6:
		pass

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
