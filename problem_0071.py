#!/usr/bin/env python3
"""
Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
It can be seen that 2/5 is the fraction immediately to the left of 3/7.
By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.
"""

import logging
import sys
import time

logging.basicConfig(
    level=logging.INFO,
    stream=sys.stderr,
    format='%(levelname)s: %(message)s')

def main():
    min_diff = None
    limit = 1000000
    (n,d) = (3,7)
    target = n/d
    for denom in range(1,limit):
        if denom%d==0: continue
        numer = (denom * n) // d
        diff = target - float(numer) / denom
        logging.debug("Target:{:4.2} - {}/{} = {:4.2}".format(target,numer,denom,diff))
        if not min_diff or min_diff[0] > diff:
            logging.debug("New diff {}/{}: {:4.2}".format(numer, denom, diff))
            min_diff = (diff, numer, denom)
    print("{}/{} -> {}/{}".format(min_diff[1],min_diff[2],n,d))

def fract_gen(gran):
    pass

def test():
    queue = [(3,1),(2,1)]
    limit = 8
    branches = [ lambda m,n:(2*m-n,m),
                 lambda m,n:(2*m+n,m),
                 lambda m,n:(m+2*n,n) ]
    while queue:
        node = queue.pop(0)
        logging.debug("{}/{}".format(node[::-1]))
        for branch in branches:
            new_node = branch(node)
            if max(new_node) <= limit:
                queue.append(new_node)

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {:4.2} seconds".format(time.time()-start))
