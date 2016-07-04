#!/usr/bin/python
"""http://projecteuler.net/problem=55"""

from time import time

def main():
	count = 0
	for i in range(10000):
		itter = 1
		for lychrel in lychrel_gen(i):
			if is_palindrome(str(lychrel)):
#				print "%d took %d iters, %d" %( i , itter, lychrel) 
				break
			if itter > 50:
				count += 1
				break
			itter += 1
	print "There are %d Lychrel numbers below 10000" % count

def lychrel_gen(num):
	while True:
		num = reverse(num) + num
		yield num

def reverse(num):
	string = ''
	for c in reversed(str(num)):
		string += c
	return int(string)

def is_palindrome(string):
	half = len(string) // 2
	for c,d in zip(string[:half],reversed(string[half:])):
		if not c == d: return False
	return True

if __name__ == "__main__":
	start = time()
	main()
	print "That took %f seconds" %(time() - start)
#	print is_palindrome("1231")
