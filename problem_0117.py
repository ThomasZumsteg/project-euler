#!/usr/bin/python
"""http://projecteuler.net/problem=117"""

from time import time

def main():
	blocks = range(2,5)
	print fill(50,blocks)

lookup = {}

def fill(l,m_set):
	if m_set[0] > l: return 1
	global lookup
	if l in lookup: return lookup[l]
	else:
		lookup[l] = fill_worker(l, m_set)
		return lookup[l]

def fill_worker(l,m_set):
	count = 1
	for m in m_set:
		for s in range(l-m+1):
			w = fill(s,m_set)
			count += w
	return count

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
