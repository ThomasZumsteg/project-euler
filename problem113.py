#!/usr/bin/python
"""http://projecteuler.net/problem=113"""

from time import time
from progressbar import ProgressBar

def main():
	power = 100
	p = power_gen()
	count = 9
	for i in range(1,power):
#		print "%d: %d" %(i, count)
#		print "%d: %d" %(i, slow_nums(i))
		num = p.next()
		count += num
	print "There are %d numbers below 10^%d"%(count,power)
	

def power_gen():
	out_q = [ [0] + range(9),
			  range(10)[::-1] ]
	while True:
		in_q = out_q
		out_q = [ [0] + range(9),
				  range(10)[::-1] ]
		count = 9
		for i, row in enumerate(in_q):
			for j, n in enumerate(row):
				mod_nums(out_q,i,j,in_q[i][j])
				count += in_q[i][j]
		yield count

def mod_nums(s,i,j,n):
	num_map = [ lambda x: range(x,10), 
				lambda x: range(x+1)  ]
	for k in num_map[i](j):
		s[i][k] += n

def slow_nums(l):
	out_q = [str(x) for x in range(1,10)]
	count = len(out_q)
	for i in range(l-1):
		in_q = out_q
		out_q = []
		for n in in_q:
			for d in [str(x) for x in range(10)]:
				m = n + d
				if sloped(m):
					out_q.append(m)
		count += len(out_q)
	return count
		
def sloped(n):
	s = ''.join(sorted(n))
	return s == n or s == n[::-1]

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
