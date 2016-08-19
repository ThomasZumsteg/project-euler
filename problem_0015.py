#!/usr/bin/python
"""Starting in the top left corner of a 2x2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20x20 grid?"""

import time

def main():
    print new_method(20,20)
#   node_map = create_map(10,10)
#   print unique_paths(len(node_map)-1, node_map)

def new_method(width,height):
    lines = [range(2,width+2)]
    for i in range(3, height+2):
        line = []
        for j,num in enumerate(lines[-1]): 
            if j == 0: value = i 
            else     : value = line[-1] + num
            line.append(value)
        lines.append(line)
    return lines[-1][-1]

def create_map(width, height):
    previous_line = [None] * width
    previous_node = None
    node_map = []
    for h in range(height+1):
        for w in range(width+1):
            if h == 0: down  = None
            else     : down  = len(node_map)-width-1
            if w == 0: right = None
            else     : right = len(node_map)-1
            node_map.append((down,right))
    return node_map

def unique_paths(root, node_map):
    queue = [root]
    path_count = 0
    while queue:
        node = node_map[queue.pop()]
        has_steps = False
        for step in node:
            if step:
                has_steps = True
                queue.append(step)
        if not has_steps:
            path_count += 1
    return path_count

if __name__ == "__main__":
    start = time.time()
    main()
    print "That took %0.5f seconds" % (time.time() - start)
