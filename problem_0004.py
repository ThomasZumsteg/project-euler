#!/usr/bin/python
"""A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 x 99.

Find the largest palindrome made from the product of two 3-digit numbers."""

def palindrome_number(digits):
	for i in list_of_n_digit_products(digits):
		if is_palindrome(i):
			return i

def list_of_n_digit_products(n):
	"""Don't use for n greater then 3"""
	largest = 10**(n) # actually 10**(n-1)-1 but range fixes it
	smallest = 10**(n-1)
	products = []
	for i in range(smallest, largest):
		for j in range(smallest,largest):
			products += [ i*j ]
	return reversed(sorted(set(products)))

def is_palindrome(a_thing):
	a_thing = str(a_thing)
	for i in range(0,len(a_thing)/2):
		if(a_thing[i] != a_thing[-i-1]):
			return False
	return True

if __name__=="__main__":
	print palindrome_number(3)
