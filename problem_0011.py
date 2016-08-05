#!/usr/bin/env python3

"""What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20x20 grid? (see problem 11.txt)"""

from re import sub
from operator import mul
from time import time

def main():
    data_set = read_file("problem_0011.txt")
    print(find_largest_multiple_of_n(data_set,4))

def read_file(file_name):
    data = []
    with open(file_name, 'r') as f:
        for line in f.readlines():
            data.append([int(x) for x in line.split(' ')])
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
    horz_mult, diag_mult, vert_mult, udag_mult = 0, 0, 0, 0

    if len(data[vert])-horz >= length:
        horz_mult = reduce(mul, data[vert][horz:horz+length])
    if len(data[vert])-horz >= length and len(data)-vert >= length:
        diag_mult = reduce(mul, get_diag(data, horz, vert, length))
        udag_mult = reduce(mul, get_udag(data, horz, vert, length))
    if len(data)-vert >= length:
        vert_mult = reduce(mul, get_vert(data, horz, vert, length))
    return max(diag_mult, vert_mult, horz_mult, udag_mult)

def reduce(operator, data):
    result = data[0]
    for d in data[1:]:
        result = operator(result, d)
    return result

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
    start = time()
    main()
    print('That took {:0.5f} seconds'.format(time() - start))
