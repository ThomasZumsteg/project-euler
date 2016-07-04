#!/usr/bin/python
"""http://projecteuler.net/problem=30"""

def main():
	fifth_power_sum = 0
	for i in range(2,360000):
		temp = [int(x)**5 for x in list(str(i))]
		if i == sum(temp):
			fifth_power_sum += i
			print i
	print fifth_power_sum

if __name__ == "__main__":
	main()
