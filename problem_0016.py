#!/usr/bin/env python3

from time import time

def main():
    exp = 1000
    base = 2
    answer = sum(int(d) for d in str(base**exp))
    print('Answer: {}'.format(answer))

if __name__ == "__main__":
    start = time()
    main()
    print('That took {:0.4} seconds'.format(time() - start))
