#!/usr/bin/python

from time import time
from common import counter

def main():
	count = 0
	for n in counter(1):
		for m in reversed(range(1,10)):
			num_digits = len(str(m**n))
			if num_digits == n:
#				ps = "%d = %d^%d"
#				print ps %(m**n, m,n)
				count += 1
			elif num_digits < n: break
		if m >= 9: break
	ps = "There are %d numbers for which the nth power has n digits"
	print ps %(count)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
