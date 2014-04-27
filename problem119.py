#!/usr/bin/python
"""http://projecteuler.net/problem=119"""

from time import time
from itertools import count
from sys import stdout

def main():
	power_sum_list = []
	for s in range(2,10000):
		for p in range(2,10):
			n = s**p
			if sum([int(d) for d in str(n)]) == s:
				power_sum_list.append(n)
	power_sum_list.sort()
	print "The 30th digit power sum is %d" %(power_sum_list[29])

def has_digit_power_sum(n):
	s = sum([int(d) for d in str(n)])
	p = s
	i = 1
	while p <= n and p != 1:
		if p == n:
			print "%d: %s: %d" %(n,s,i)
			return True
		p *= s
		i += 1
	return False

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
