#!/usr/bin/python
"""http://projecteuler.net/problem=90"""

from time import time
from itertools import combinations

def main():
	sets = set()
	for n_set in combinations(range(10),6):
		for m_set in combinations(range(10),6):
			if is_valid(list(n_set),list(m_set)):
				n_str = ''.join(str(n) for n in n_set)
				m_str = ''.join(str(m) for m in m_set)
				mn = frozenset((m_str,n_str))
				sets.add(mn)
	print "There are %d valid sets" %(len(sets))

def is_valid(n_set, m_set):
	# 6 and 9 are equivalent
	squares = [(0,1), # 1^2 = 01
			   (0,4), # 2^2 = 04
			   (0,6), # 3^2 = 09
			   (1,6), # 4^2 = 16
			   (2,5), # 5^2 = 25
			   (3,6), # 6^2 = 36
#			   (4,6), # 7^2 = 49
			   (6,4), # 8^2 = 64
			   (8,1)] # 9^2 = 81
	for mn_set in [n_set, m_set]:
		try: 
			i = mn_set.index(9)
			mn_set[i] = 6
		except ValueError: pass
	for n,m in squares:
		forward = (n in n_set) and (m in m_set)
		backward= (m in n_set) and (n in m_set)
		if not (forward or backward):
			return False
	return True

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
