#!/usr/bin/python
"""A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers."""

from common import factors_of_num

def main():
	abundant_list = []
	nums = range(28124)
	for i in range(12,28123):
		if is_abundant(i):
			abundant_list.append(i)
	for i in range(len(abundant_list)):
		for j in range(i+1):
			a = abundant_list[i]
			b = abundant_list[j]
			if a+b < len(nums):
				nums[a+b] = 0
	print sum(nums)

def is_abundant(num):
	if sum(factors_of_num(num)) > num:
		return True
	return False

if __name__ == "__main__":
	main()
