#!/usr/bin/python
"""http://projecteuler.net/problem=92"""

from time import time
from progressbar import ProgressBar

def main():
	progress = ProgressBar()
	big_num = 10000000
	is_unhappy = [None] * 600
	is_unhappy[0] = False
	is_unhappy[4] = True
	is_unhappy[1] = False
	for i in progress(range(big_num)):
		n = i
		queue = []
		while n >= 600 or is_unhappy[n] is None:
			queue.append(n)
			n = sum_digit_sqaures(n, is_unhappy)
		happy_val = is_unhappy[n]
#		s = ""
		for num in queue:
			try:
				is_unhappy[num] = happy_val
			except IndexError: pass
#			s += "%d -> " % num
#		print s + str(happy_val)
	print "%d number are unhappy :(" %(is_unhappy.count(True))

def sum_digit_sqaures(n, is_unhappy):
	try:
		if is_unhappy[n] is not None:
			return is_unhappy[n]
	except IndexError: pass
	return sum_digit_sqaures_helper(n)

def sum_digit_sqaures_helper(n):
	sqr_digit_sum = 0
	for d in str(n):
		sqr_digit_sum += int(d)**2
	return sqr_digit_sum

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %( time() - start)
