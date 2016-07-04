#!/usr/bin/python
"""http://projecteuler.net/problem=95"""

from time import time

def main():
	size = 1000000
	proper_sums = get_proper_sums(size)
	max_len = 0
	for i in range(1,size+1):
		chain = find_chain(i,proper_sums)
		if len(chain) > max_len:
			max_len = len(chain)
			max_chain = chain
	print "Longest chain is %d with a minimum link %d" %(max_len, min(max_chain))


def find_chain(i,sums):
	size = len(sums)
	queue = []
	while sums[i]:
		queue.append(i)
		j = sums[i]
		sums[i] = False
		i = j
		if i > size: return []
	try:
		j = queue.index(i)
		return queue[j:]
	except ValueError:
		return []

def get_proper_sums(n):
	n += 1
	sieve = [1] * n 
	for i in range(2,n):
		for j in range(2*i,n,i):
			sieve[j] += i
	return sieve

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
