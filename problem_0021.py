#!/usr/bin/python

from common import factors_of_num

def main():
	amicable_sum = 0
	for num in range(10000):
		if is_amicable(num):
			amicable_sum += num
	print amicable_sum 

def is_amicable(num):
	a_pair = sum(factors_of_num(num))
	if a_pair == num: return False
	b_pair = sum(factors_of_num(a_pair))
	if b_pair == num: 
		print "%d :%d" %(a_pair, b_pair)
		return True
	return False

if __name__ == "__main__":
	main()
