#!/usr/bin/python
"""http://projecteuler.net/problem=74"""

from time import time
from progressbar import ProgressBar
import sys

def main():
	p = ProgressBar()
	count = 0
	lim = 1000000
	stop = 60
	fact = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
	loops= { 145:1,
			 169:3,
		  363601:3,
		    1454:3,
			 871:2,
		   45361:2,
		     872:2,
		   45362:2,
		       1:1,
			   2:2 }
	for n in p(range(3,lim+1)):
		lenth = chain_len(n,loops,fact, stop) 
		if lenth == stop:
			count += 1
	print "There are %d chains of lenth 60" % count

def chain_len(num, loops, fact, lim):
#	sys.stdout.write( "Start: " + str(num) )
	chain = []
	while num not in loops:
		if len(chain) > lim: return 
		chain.append(num)
#		sys.stdout.write(" " + str(num))
		num = sum([fact[int(i)] for i in list(str(num))])
#	sys.stdout.write("\n")
	loop_num = loops[num]
	for i,n in enumerate(reversed(chain)):
		loops[n] = i + loop_num + 1
	return len(chain) + loops[num]

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
