#!/usr/bin/python
"""http://projecteuler.net/problem=72"""

from time import time
from progressbar import ProgressBar

def main():
	lim = 1000000
	p_bar = ProgressBar(maxval = lim-1).start()
	count = 0
	for i in range(lim-1):
		if not sieve[i]:
			for j in range(i,lim-1,i+2):
				sieve[j].add(i+2)
		count += phi(i+2,sieve[i])
		p_bar.update(i)
	p_bar.finish()
	print "There are %d proper reduced fractions" % count

def phi(num,factors):
	val = num
	for f in factors:
		val *= (1 - 1.0/f)
	return int(val)

def brute(limit):
	count = 0
	queue = set( [(2,1), (3,1)] )
	branches = [ lambda (m,n): (2*m-n,m),
				 lambda (m,n): (2*m+n,m),
				 lambda (m,n): (m+2*n,n) ]
	while queue:
		node = queue.pop()
#		print "%d/%d" %node[::-1]
		count += 1	
		for branch in branches:
			leaf = branch(node)
			if leaf[0] <= limit:
				queue.add(leaf)
	return count

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() -start)
