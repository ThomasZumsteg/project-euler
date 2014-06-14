#!/usr/bin/python

from time import time
from sys import stdout
from itertools import count

def main():
	slow_find(10)
	nums = fast_find(10)
	print "There are %d solutions" %(nums.count(1))
	#n = 10000000
	#factors = factor_sieve(n)
#	print factors

def factor_sieve(n):
	factors = [True] * n
	factors[0:2] = [False, False]
	for m in range(2,n//2+1):
		if factors[m]:
			for i in range(2 * m, n, m):
				factors[i] = False
				stdout.write("%d: %d\r" %(m, i))
				stdout.flush()
	return factors

def fast_find(big_num):
	nums = [0] * big_num
	for y in range(2, big_num - 1):
		for s in count((y - 1) % 4 + 1, 4):
			try:
				nums[y * s] += 1
				a = (s + y) // 4
				print "(%d, %d) (%d, %d, %d) = %d" %(a,y,y + a, y, y - a, s * y)
			except IndexError:
				#print "Done (%d, %d) = %d" %(y,s,y*s)
				break
	return nums

def slow_find(big_num):
	target = 1
	nums = [set() for _ in range(big_num)]
	for m in range(1, big_num // 2 + 1):
		for b in gap_range(m, big_num):
			(x,y,z) = (b+m, b, b-m)
			n = 4 * m * b - b ** 2
#			print "%3d = %2d^2 - %2d^2 - %2d^2" %(n,x,y,z)
			nums[n].add((x,y,z))
	values = 0
	for i,n in enumerate(nums):
		if target == len(n):
			print "%d: %d: %s" %(i, len(n), n)
			values += 1
		#else: print "%d: %d: %s" %(i, len(n), n)
	print "There are %d solutions" %(values)

def gap_range(m, big_num):
	diff =  4 * m ** 2 - big_num
#	line = "-" * 10 + " %s " + "-" * 10
#	print line % ("start: %2d" % m)
	if diff < 0:
		for c in range(m + 1, 4 * m):
			yield c
	else:
		gap_start = 2 * m - int(diff ** 0.5)
		gap_end   = 2 * m + int(diff ** 0.5)
		for a in range(m + 1, gap_start):
			yield a
#		print line % ("break: %2d" % m)
		for b in range(gap_end + 1, 4 * m):
			yield b
#	print line % ("end:   %2d" % m)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
