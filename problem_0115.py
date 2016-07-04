#!/usr/bin/python
"""http://projecteuler.net/problem=115"""

from time import time
from itertools import count
from sys import stdout

def main():
	big_num = 1000000000
	m = 5000
	for n in count(1):
		f = fill(n,m)
		stdout.write("%d: %d\r" % (n,f))
		stdout.flush()
		if f > big_num:
			print "F(%d, %d)=%d > %d" %(n,m,f,big_num)
			return

lookup = {}

def fill(l,m):
	if l < m:
		return 1
	global lookup
	if (l,m) in lookup:
		return lookup[(l,m)]
	else:
		w = fill_worker(l,m)
		lookup[(l,m)] = w
		return w

def fill_worker(l,m):
	count = 1
	for s in range(1,l-m+2):
		count += s * fill(l-m-s,m)
	return count

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
