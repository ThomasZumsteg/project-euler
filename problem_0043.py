#!/usr/bin/python
"""http://projecteuler.net/problem=43"""

from common import lexicographic_gen

def main():
	pan_sum = 0
	for i in lexicographic_gen('0123456789'):
		if has_property(i):
			print "%s has property" %i
			pan_sum += int(i)
	print pan_sum

def has_property(num):
	fact_list = [ 2, 3, 5, 7,11,13,17]
	for i in range(1,8):
		mod = int(num[i:i+3]) % fact_list[i-1]
#		print "%s %% %d = %d" %(num[i:i+3],fact_list[i-1],mod)
		if int(num[i:i+3]) % fact_list[i-1] != 0:
			return False
	return True

if __name__ == "__main__":
	main()
