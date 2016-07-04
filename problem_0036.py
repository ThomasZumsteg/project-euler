#!/usr/bin/python
"""http://projecteuler.net/problem=36"""

def main():
	pal_sum = 0
	for i in range(1,int(1e6)):
		int_is_pal = is_palindrome(str(i))
		bin_is_pal = is_palindrome(bin(i)[2:])
		if bin_is_pal and int_is_pal:
			pal_sum += i
			print "This is a palindrome %d: %s" %(i,bin(i))
	print "The sum of all palindromes below 1 million is %d" % pal_sum

def is_palindrome(string):
	half_way = len(string)/2
	for a,b in zip(string[:half_way],reversed(string[half_way:])):
		if a != b: return False
	return True

if __name__ == "__main__":
	main()
