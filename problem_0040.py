#!/usr/bin/python
"""http://projecteuler.net/problem=40"""

def main():
	counter = 1
	string = ''
	while len(string) < 1000010:
		string += str(counter)
#		print string
		counter += 1
	product = 1
#	print string[11:14]
	for i in [1,10,100,1000,10000,100000,1000000]:
#		print string[i-1]
		product *= int(string[i-1])
	print product

if __name__ == "__main__":
    main()
