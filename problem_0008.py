#!/usr/bin/python

from re import sub

def consecutive_digit_product(consecutive, digits):
	max_product = 0
	index = 0
	for digit_group in get_n_digit_group(consecutive,digits):
		product = 1
		for digit in digit_group:
			product *= int(digit)
		if product > max_product:
			max_product = product
	return max_product

def get_n_digit_group(n, digits):
	digit_file = open(digits)
	text = digit_file.read()
	digit_file.close()
	text = sub("\D", "", text)
	i = 0
	while True:
		yield text[i:(i+n)]
		i += 1
		if i+n > len(text): break

if __name__ == "__main__":
	print consecutive_digit_product(5,"problem8.txt") # 180
