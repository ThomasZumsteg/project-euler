#!/usr/bin/python

def main():
	big_num = factorial(100)
	big_num = list(str(big_num))
	big_sum = 0
	for num in big_num:
		big_sum += int(num)
	print big_sum

def factorial(n):
	result = 1
	for i in range(1,n+1):
		result *= i
	return result

if __name__ == "__main__":
	main()
