#!/usr/bin/python
"""http://projecteuler.net/problem=112"""

from time import time
from itertools import count
from sys import stdout

def main():
	yes = 0
	for n in count(1):
		if not(sloped(str(n)) or sloped(str(n)[::-1])):
			yes += 1
		if yes == n * 0.99:
			print "\n%d" %n 
			break
		stdout.write("%4d:%4d %f\r" %(n,yes,float(yes)/n))
		stdout.flush()

def sloped(n):
	for i in range(1,len(n)):
		if n[i-1] > n[i]:
			return False
	return True

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
