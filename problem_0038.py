#!/usr/bin/python
"""http://projecteuler.net/problem=38"""

from common import counter

def main():
	max_concat = 0
	for i in range(1,10000):
#		print "New %d" %i
		concat_num = ''
		for j in counter(1):
			concat_num += str(i*j)
#			print "Testing %s" % concat_num
			if len(concat_num) > 9:
#				print "Failed"
				break
			elif len(concat_num) == 9:
				if is_pandigital(concat_num):
					print "Found pandigital %d: %s" %(i, concat_num)
					max_concat = max(max_concat, int(concat_num))
	print max_concat

def is_pandigital(string):
	for i in range(1,10):
		if string.count(str(i)) != 1:
			return False
	return True

if __name__ == "__main__":
	main()
