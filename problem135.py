#!/usr/bin/python
"""http://projecteuler.net/problem=135"""

from time import time
from itertools import count
from sys import stdout

def main():
	big_num = 100000
	nums = [0] * big_num
	for m in range(1,big_num):
		for b in count(1):
			(x,y,z) = (2*m+b, m+b, b)
			n = x**2-y**2-z**2
			#print "%2d: %2d: %3d" %(m,b,n)
			if n <= 0 : break
			if n >= big_num: continue
			nums[n] += 1


if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)