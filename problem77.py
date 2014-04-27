#!/usr/bin/python
"""http://projecteuler.net/problem=77"""

from time import time
from progressbar import ProgressBar
from common import prime_sieve

def main():
	block = 10000
	n = 3
	lim = 5000000
	prime_list = prime_sieve(block)
	p_bar = ProgressBar(maxval=lim).start()
	while True:
		ways = changes(n,prime_list)
#		print "%d: %d: %s" %(n,ways,str(prime_list))
		if ways > lim:
			p_bar.finish()
			print "%d can be written over %d ways" %(n,lim)
			return
		p_bar.update(ways)
		n += 1
		if prime_list[-1] < n:
			new_primes = prime_sieve(i+block,prime_list)
			prime_list.append(new_primes)

def changes(amount, coins):
	ways = [0] * (amount + 1)
	ways[0] = 1
	for coin in coins:
		for j in range(coin, amount + 1):
			ways[j] += ways[j-coin]
	return ways[amount]

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
