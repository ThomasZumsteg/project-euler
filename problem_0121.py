#!/usr/bin/python
"""http://projecteuler.net/problem=121"""

from time import time
from itertools import combinations

def main():
	turns = 15
	prob = [[None]*turns for x in (1,2)]
	prob_sum = 0
	for i in range(turns):
		prob[0][i] =     1.0 / (2+i)
		prob[1][i] = (1.0+i) / (2+i)
	for n in range((turns+1)//2):
		for c in combinations(range(turns),n):
			prod = 1
			for i in range(turns):
				if i in c:
					prod *= prob[1][i]
				else:
					prod *= prob[0][i]
			prob_sum += prod
	lb = int((1-prob_sum)/prob_sum) + 1
	print "The vendor should reward %d" %(lb)

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
