#!/usr/bin/python
"""http://projecteuler.net/problem=93"""

from time import time
from operator import add, sub, mul, div
from itertools import combinations, count
from sys import stdout

def main():
	max_this = 0
	for digits in combinations(range(10),4):
		s_digits = ''.join(str(d) for d in digits)
		this = consecutive_int(digits)
		stdout.write("Testing %s: %d\r" %(s_digits, this))
		stdout.flush()
		if this >= max_this:
			max_this = this
			m_digit = s_digits
	print "%s has %d consecutive integers" %(m_digit, max_this)

def consecutive_int(digits):
	ints = set()
	# first operation can choose two from 1234
	# second can choose two from 123
	# third can only choose 12
	queue = [digits]
	while queue:
		node = queue.pop()
		if len(node) == 1 and valid(node[0]):
			ints.add(int(node[0]))
		else:
			add_to_queue(queue, node)
	# find longest string of ints
	for n in count(1):
		if n not in ints:
			return n-1

def add_to_queue(queue, nums):
	operations = [add, sub, mul, div, r_sub, r_div]
	for a,b in combinations(nums,2):
		for o in operations:
			try:
				c = o(float(a),b)
				node = [c]
				for n in nums:
					if not (n==a or n==b):
						node.append(n)
				queue.append(tuple(node))
			except ZeroDivisionError: pass

def valid(total):
	if not int(total) == total: return False
	if total < 0: return False
	return True

def r_sub(a,b):
	return sub(b,a)

def r_div(a,b):
	return div(b,a)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
