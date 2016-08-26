#!/usr/bin/env python3
"""By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

    (See problem18_data.txt

    NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)"""

from time import time

def main():
    tree = read_array("problem_0018.txt")
    print(brute_force(0,tree))

def brute_force(root, tree):
    queue = [(root,0)]
    max_sum = 0
    while queue:
        [index,val] = queue.pop()
        if index >= len(tree):
            if max_sum < val: max_sum = val
            continue 
        val += tree[index][0]
        for step in tree[index][1:]:
            queue.append((step,val))
    return max_sum

def add_up(file_name):
    file_handle = open(file_name)
    p_line = []
    for line in reversed(file_handle.readlines()):
        line = [int(i) for i in line.split()]
        for i, val in enumerate(line):
            if p_line:
                line[i] += max(p_line[i],p_line[i+1])
        p_line = line
    return p_line[0]

def num_tree_lines(tree):
    count_sum = 0
    count = 1
    tree_len = len(tree)
    while True:
        count_sum += count
        if count_sum >= tree_len:
            return count
        count += 1

def read_array(text_file):
    file_handle = open(text_file)
    tree = []
    left = 1
    for line in file_handle:
        line = line.split()
        line = [int(x) for x in line]
        for val in line:
            tree.append([val,left,left+1])
            left += 1
        left += 1
    return tree

if __name__=="__main__":
    start = time()
    print('Answer: {}'.format(add_up("problem_0018.txt")))
    finish = time()
    print('Quick solve took {:0.4f} seconds'.format(finish - start))
    main()
    print('Brute force took {:0.4f} seconds'.format(time() - finish))
