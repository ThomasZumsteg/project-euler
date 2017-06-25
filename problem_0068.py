#!/usr/bin/env python3
"""
Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.

Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.

It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
Total	Solution Set
9	4,2,3; 5,3,1; 6,1,2
9	4,3,2; 6,2,1; 5,1,3
10	2,3,5; 4,5,1; 6,1,3
10	2,5,3; 6,3,1; 4,1,5
11	1,4,6; 3,6,2; 5,2,4
11	1,6,4; 5,4,2; 3,2,6
12	1,5,6; 2,6,4; 3,4,5
12	1,6,5; 3,5,4; 2,4,6

By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.

Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings. What is the maximum 16-digit string for a "magic" 5-gon ring?
"""

import logging
import sys
from time import time


logging.basicConfig(
    level=logging.DEBUG,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    n = 5
    max_gon = 0
    for gon in magic_gon_gen(n):
        logging.debug(gon)
        if int(gon) > max_gon and len(gon) == 16:
            max_gon = int(gon)
    print("Maximum magic gon is {}".format(max_gon))

def magic_gon_gen(n):
    (gon, elem) = make_gon(n)
    init(elem[:n])
    while True:
        for e in elem[n:]:
            e[0] = 0
        if can_set_outside(gon, elem):
            yield str_gon(gon)
        if not increment_inside(elem[:n]): break

def init(elem):
    for i,e in enumerate(elem):
        e[0] = i + 1

def increment_inside(i_elems):
    l = len(i_elems)
    n = 2 * l
    i = l - 1   # start at the end
    while True:
        e = i_elems[i]
        e[0] += 1
        if i_elems.count(e) == 1 and e[0] <= n:
            if i + 1 == l: break
            else: i += 1
        elif e[0] > n:
            e[0] = i_elems[0][0]
            i -= 1
            if i < 0: return False
#       print i_elems
    return True

def can_set_outside(g,e):
    n = len(e) // 2
    inner_sum = sum([x[0] for x in e[:n]])
    gon_sum = inner_sum / n + (2 * n + 1)
    if inner_sum % n != 0: return False
    logging.debug("e[:n]:{}".format(str(e[:n])))
    logging.debug("n:{}, inner_sum:{}, gon_sum:{}".format(n,inner_sum, gon_sum))
    # determine sum
    for line in g:
        line_sum = sum([x[0] for x in line[1:]])
        if line_sum >= gon_sum: return False
        val = gon_sum - line_sum
        if val > 2 * n: return False
        elif e.count([val]) >= 1: return False
        line[0][0] = val
    return True

def str_gon(g):
    i     = 0
    small = None
    for j,line in enumerate(g):
        if not small or small > line[0][0]:
            small = line[0][0]
            i = j
    g = g[i:] + g[:i]
    g_str = []
    for i in range(len(g)):
        g_str.append(''.join([str(x[0]) for x in g[i]]))
    return ''.join(g_str)

def make_gon(n):
    elements = [[e] for e in [0] * (n * 2)]
    gon = []
    for i in range(n):
        g_line = [elements[i+n],elements[i],elements[i+1]]
        logging.debug("[elements[{}],elements[{}],elements[{}]]".format(i,i+1,i+n))
        gon.append(g_line)
    gon[-1][2] = elements[0]
    return (gon, elements)

if __name__ == "__main__":
    start = time()
    main()
    logging.info("That took {:4.2f} seconds".format( time() - start ))
