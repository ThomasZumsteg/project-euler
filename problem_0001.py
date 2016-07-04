#!/usr/bin/python

def sum_of_multiples(big_num):
	debug = False
	sum_multiple = 0
	for i in range(0,big_num):
		if i % 3 == 0 or i % 5 == 0:
			sum_multiple += i
			print "adding %d to %d" %(i, sum_multiple)
	return sum_multiple

if __name__ == "__main__":
	print sum_of_multiples(1000)
