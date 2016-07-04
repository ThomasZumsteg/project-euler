#!/usr/bin/python
"""http://projecteuler.net/problem=67"""

from time import time
from problem18 import add_up

def main():
	n_max = add_up('problem67.txt')
	print "%d is the max path sum" % n_max

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %( time() - start )
