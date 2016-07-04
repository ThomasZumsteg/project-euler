#!/usr/bin/python

"""Work out the first ten digits of the sum of the following one-hundred 50-digit numbers."""

def main():
	file_handle = open("problem13.txt")
	nums = []
	for line in file_handle:
		nums.append(int(line))
	nums_sum = str(sum(nums))
	print nums_sum[:10]

if __name__ == "__main__": 
	main()
