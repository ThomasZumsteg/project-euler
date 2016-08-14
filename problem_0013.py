#!/usr/bin/env python3

"""Work out the first ten digits of the sum of the following one-hundred 50-digit numbers."""

from time import time

def main():
    with open("problem_0013.txt") as f:
        total = sum(int(l) for l in f.readlines())
    print('Answer: {}'.format(str(total)[:10]))

if __name__ == "__main__":
    start = time()
    main()
    print('That took {:1.5f} seconds'.format(time() - start))
