#!/usr/bin/python
"""http://projecteuler.net/problem=85"""

from time import time

def main():
	goal = 2e6
	min_diff = (None, 0)
	for m in range(1,100):
		for n in range(m,100):
			mn_sub_diff = abs(goal - sub_rects(m,n))
			if not min_diff[0] or min_diff[0] > mn_sub_diff:
				min_diff = (mn_sub_diff, m*n)
	print "Min diff of %d for an area of %d" % min_diff

def sub_rects(m,n):
	return int((m/2.0)*(m+1)*(n/2.0)*(n+1))

if __name__=="__main__":
	start = time()
	main()
	print "That took %f seconds" %( time() - start )
