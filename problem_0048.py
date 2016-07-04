#!/usr/bin/python
"""http://projecteuler.net/problem=48"""

def main():
	num_sum = 0
	for num in reversed(range(1,1001)):
		digits = num**num % int(1e10)
		num_sum += digits
	num_sum = num_sum % int(1e10)
	print num_sum

if __name__ == "__main__":
	main()
