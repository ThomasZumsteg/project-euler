#!/usr/bin/python

def sum_square_difference(the_list):
	sum_square = 0
	square_sum = 0
	for i in the_list:
		square_sum += i**2
		sum_square += i
	return sum_square**2 - square_sum

if __name__ == "__main__":
	the_list = range(1,101)
	print sum_square_difference(the_list)
