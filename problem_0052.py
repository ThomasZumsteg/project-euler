#!/usr/bin/python
"""http://projecteuler.net/problem=52"""

from time import time
from common import counter

def main():
	for i in counter(1):
		digits = sorted(list(str(i)))
		done = True
		for j in range(2,7):
			if not digits == sorted(list(str(i*j))):
				done = False
				break
		if done: break
	print i

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
