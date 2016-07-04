#!/usr/bin/python
"""http://projecteuler.net/problem=135"""

from time import time
from itertools import count
from sys import stdout

def main():
	big_num = 1000000
	target = 10
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
#			print "%d: %s" %(i,n)
			values += 1
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
