#!/usr/bin/env python3

from time import time

def main():
	big_num = factorial(100)
	return sum(int(d) for d in str(big_num))

def factorial(n):
	result = 1
	for i in range(1,n+1):
		result *= i
	return result

if __name__ == "__main__":
    start = time()
    print('Answer: {}'.format(main()))
    print('That took {:0.3} seconds'.format(time() - start))
