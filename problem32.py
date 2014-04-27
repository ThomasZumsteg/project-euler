#!/usr/bin/python
"""http://projecteuler.net/problem=32"""

from re import sub
from common import lexicographic_gen

def main():
	product_list = []
	for order in lexicographic_gen('123456789'):
		for i in range(1,len(order)-1):
			for j in range(i+1,len(order)):
#				print "%s x %s = %s" %(str((0,i)),str((i,j)),str((j,len(order))))
#				print "%s x %s = %s" %(order[:i],order[i:j],order[j:])
				a = int(order[:i])
				b = int(order[i:j])
				c = int(order[j:] )
				product = a * b
				if product > c:
					break
				if product == c:
					print "%d x %d = %d" %(a,b,c)
					product_list.append(product)
	print sum(list(set(product_list)))

def pair_gen(digits):
	pass

def is_pandigital(digits, n=9):
	for i in range(1,n+1):
		if not digits.count(i) == 1:
			return False
	for i in range(n+1,10):
		if not digits.count(i) == 0:
			return False
	return True

if __name__ == "__main__":
	main()
