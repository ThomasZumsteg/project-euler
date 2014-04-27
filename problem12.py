#!/usr/bin/python

from math import sqrt

def main():
	i = 0
	n = 0
	for triangle_num in triangle_number_gen():
		num_factors_in_num = num_factors(triangle_num)
#		num_factors_in_num = 0
		i += 1
		if num_factors_in_num > 500:
			print "%d: %d: %d" %(n*1000, triangle_num, num_factors_in_num)
			break
		if i >= 1000:
			i = 0
			n += 1
			print "%d: %d: %d" %(n*1000, triangle_num, num_factors_in_num)

def num_factors(number):
	num_factors = 0
	for i in range(1,int(sqrt(number))+1):
		if number % i == 0:
			num_factors += 2
	return num_factors

def triangle_number_gen():
	triangle_num = 1
	incrementer = 2
	while True:
		yield triangle_num
		triangle_num += incrementer
		incrementer += 1

if __name__ == "__main__":
	main()
