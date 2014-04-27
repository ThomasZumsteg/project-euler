#!/usr/bin/python
"""http://projecteuler.net/problem=53"""

from time import time

def main():
	count = 0
	for n in range(1,101):
		for r in range(n):
			combinations = fact(n)/(fact(r)*fact(n-r))
			if combinations > 1e6:
#				print "There are %d ways of selection %d from %d" %(combinations, r, n)
#				return None
				count += 1
	print count

def fact(num):
	product = 1
	for i in range(1,num+1):
		product *= i
	return product

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
