#!/usr/bin/python
"""http://projecteuler.net/problem=73"""

from time import time
from progressbar import ProgressBar

def main():
	p = ProgressBar()
	lim = 12000
	count = 0
	(start, end) = (1.0/3, 1.0/2)
	for i in p(range(3,lim+1)):
		upper = int((i-0.5)//2+1)
		lower = int(i//3 + 1)
		for j in range(lower,upper):
			if gcf(i,j) == 1: 
				count += 1
	print "There are %d fractions between 1/3 and 1/2" % count


def gcf(high, low):
	while low != 0:
		(high, low) = (low, high % low)
	return high

def brute(limit):
	count = 0
	in_range = 0
	queue = [(2,1), (3,1)]
	branches = [ lambda (m,n): (2*m-n,m),
				 lambda (m,n): (2*m+n,m),
				 lambda (m,n): (m+2*n,n) ]
	while queue:
		node = queue.pop(0)
#		print "%d/%d" %node[::-1]
		count += 1	
		fract = float(node[1])/node[0]
		if fract < 1.0/2 and fract > 1.0/3:
			in_range += 1
		for branch in branches:
			leaf = branch(node)
			if leaf[0] <= limit:
				queue.append(leaf)
	print "limit:%d, count:%d, range:%d" %(limit,count,in_range)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
