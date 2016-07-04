#!/usr/bin/python

def main():
	exp = 1000
	number = list(str(2**exp))
	number = [int(x) for x in number]
	print sum(number)

if __name__ == "__main__":
	main()
