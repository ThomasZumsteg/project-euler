#!/usr/bin/python
"""http://projecteuler.net/problem=97"""

from time import time
from progressbar import ProgressBar

def main():
	p = ProgressBar()
	num = 28433
	for i in p(range(7830457)):
		num *= 2
		num = num % 10000000000
	print num + 1

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
