#!/usr/bin/python
"""http://projecteuler.net/problem=62"""

from time import time
from common import counter

def main():
	digits = {}
	n = 0
	num_perm = 5
	while True:
		n += 1
		sn = digit_hash(n**3)
		add_key_value(digits, sn, n)
		m = n
		limit = int(sn[::-1])**(1.0/3)
		while len(digits[sn]) <= num_perm and m < limit:
			m += 1
			sm = digit_hash(m**3)
			add_key_value(digits, sm, m)
		
		if len(digits[sn]) == num_perm:
			items = digits[sn]
			print_str = "%d has %d cubeic permutations: %s"
			print print_str %(min(items)**3, len(items), str(items))
			return 

def digit_hash(n):
	return ''.join(sorted(list(str(n))))

def add_key_value(d,k,v):
	try: d[k].add(v)
	except KeyError:
		d[k] = set([v])

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %( time() - start)
