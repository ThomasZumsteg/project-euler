#!/usr/bin/python
"""http://projecteuler.net/problem=122"""

from time import time
from sys import stdout
from itertools import combinations_with_replacement as cwr

def main():
	m_sum = 0
	for k in range(1,201):
		m = m_func(k)
		print "%d: %d" %(k,m)
		m_sum += m
	print "The sum is %d" %(m_sum)

def m_func(num):
	if num == 1: return 0
	qout = set([frozenset([1])])
	step = 1
	while True:
		qin  = qout.copy()
		qout = set()
		for s in qin:
			for (i,j) in cwr(s,2):
#				print "%d: %d" %(i,j)
				ij = i + j
				if ij < num:
					qout.add(s | set([ij]))
				elif ij == num:
					return step
		step += 1

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
