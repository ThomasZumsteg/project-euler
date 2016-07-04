#!/usr/bin/python
"""http://projecteuler.net/problem=99"""

from time import time
from math import log

def main():
	biggest = (0, None)
	f_name = "problem99.txt"
	for i,line in enumerate(open(f_name)):
		(n,e) = [int(x) for x in line.split(',')]
#		print "%d: %f" %(i+1,e*log(n))
		biggest = max((e*log(n),i+1),biggest)
	print "Largest number is on line %d" %(biggest[1])

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time()-start)
