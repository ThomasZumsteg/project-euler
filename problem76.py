#!/usr/bin/python
"""http://projecteuler.net/problem=76"""

from time import time
from progressbar import ProgressBar

def main():
	num = 100
	print changes(num, range(1,num))
	
def changes(amount, coins):
	ways = [0] * (amount + 1)
	ways[0] = 1
	p = ProgressBar()
	for coin in p(coins):
		for j in range(coin, amount + 1):
			ways[j] += ways[j-coin]
	return ways[amount]

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
