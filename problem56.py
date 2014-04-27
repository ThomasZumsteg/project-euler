#!/usr/bin/python
"""http://projecteuler.net/problem=56"""

from time import time

def main():
	max_digit_sum = 0
	for a in range(100):
		for b in range(100):
			max_digit_sum = max(max_digit_sum, digit_sum(a**b))
	print "Largest digit sum is %d" % max_digit_sum

def digit_sum(num):
	sum_digits = 0
	for c in str(num):
		sum_digits += int(c)
	return sum_digits

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
