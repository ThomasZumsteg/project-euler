#!/usr/bin/python
"""http://projecteuler.net/problem=100"""

from time import time
from itertools import count
from sys import stdout


def main():
	i = 0
	big_num = 1000000000000
	for n,b in pell_gen():
		stdout.write("%15d: %15d\r" %(n,b))
		stdout.flush()
		if n > big_num: break
	print "For %d there are %d blue plates" %(n,b)

def pell_gen():
	(x,y) = (1,1)
	f = lambda N: (N+1)//2
	while True:
		yield f(x),f(y)
		(x,y) = (3*x+4*y, 2*x+3*y)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
