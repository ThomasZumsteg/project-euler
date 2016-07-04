#!/usr/bin/python
"""http://projecteuler.net/problem=34"""

def main():
	factorial_sum = 0
	for i in range(3,2540161):
		factorial = digit_fact(i)
		if i == factorial:
			factorial_sum += i
			print i
	print factorial_sum

def digit_fact(num):
	num = list(str(num))
	num_sum = 0
	for i in num:
		num_sum += fact(int(i))
	return num_sum

def fact(num):
	fact_sum = 1
	for i in range(1,num+1):
		fact_sum *= i
	return fact_sum

if __name__ == "__main__":
	main()
