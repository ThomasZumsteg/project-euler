#!/usr/bin/python

"""What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20x20 grid? (see problem 11.txt)"""

from re import sub

def main():
	data_set = read_file("problem11.txt")
	print find_largest_multiple_of_n(data_set,4)
	
def read_file(file_name):
	file_handle = open(file_name)
	data = []
	for line in file_handle:
		line = line.split(' ')
		line = [int(x) for x in line]
		data.append(line)
	return data

def find_largest_multiple_of_n(data,n):
	max_multiple = 0
	for i in range(len(data)):
		for j in range(len(data[i])):
			this_multiple = evaluate(data,j,i,n)
			if this_multiple > max_multiple:
				max_multiple = this_multiple
	return max_multiple

def evaluate(data,horz,vert,length):
	multiply = lambda x,y: x*y
	[horz_mult, diag_mult, vert_mult, udag_mult] = [None]*4

#	for line in data[vert:]:
#		print line[horz:]
#	print ""

	if len(data[vert])-horz >= length:
		horz_mult = reduce(multiply, data[vert][horz:horz+length])
	if len(data[vert])-horz >= length and len(data)-vert >= length:
		diag_mult = reduce(multiply, get_diag(data, horz, vert, length))
		udag_mult = reduce(multiply, get_udag(data, horz, vert, length))
	if len(data)-vert >= length:
		vert_mult = reduce(multiply, get_vert(data, horz, vert, length))
	
	return max(diag_mult, vert_mult, horz_mult, udag_mult)

def get_vert(data, horz, vert, length):
	vert_list = []
	for i in range(length):
		vert_list.append(data[vert+i][horz])
	return vert_list

def get_diag(data, horz, vert, length):
	diag_list = []
	for i in range(length):
		diag_list.append(data[vert + i][horz + i])
	return diag_list

def get_udag(data, horz, vert, length):
	udag_list = []
	for i in range(length):
		udag_list.append(data[vert + length - 1 - i][horz + i ])
	return udag_list

if __name__ == "__main__":
	main()
