#!/usr/bin/python
"""http://projecteuler.net/problem=57"""

from time import time

def main():
	i = 1000
	count = 0
	for n, d in root_fract_gen():
#		print "%d/%d" %(n,d)
		if len(str(n)) > len(str(d)): count += 1
		if i < 0: break
		i -= 1
	print "There are %d fraction with the num" % count

def root_fract_gen():
	num = 1
	den = 1
	while True:
		(num, den) = (num + 2 * den, num + den)
		yield (num, den)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" % ( time() - start )
