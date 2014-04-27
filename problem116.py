#!/usr/bin/python
"""http://projecteuler.net/problem=116"""

from time import time

def main():
	ways = 0
	l = 50
	for n in range(2,5):
		w = fill(l,n)-1
		print "%d: %d" %(n,w)
		ways += w
	print "There are %d ways" % ways

lookup = {}

def fill(l,m):
	if l < m: return 1
	global lookup
	if (l,m) in lookup: return lookup[(l,m)]
	else:
		w = fill_worker(l,m)
		lookup[(l,m)] = w
		return w

def fill_worker(l,m):
	count = 1
	for s in range(l-m+1):
		w = fill(s,m)
#		print "(%d,%d): %d" %(s,m,w)
		count += w
	return count

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
