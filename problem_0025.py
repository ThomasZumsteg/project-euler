#!/usr/bin/env python3
"""The Fibonacci sequence is defined by the recurrence relation:
Fn = Fn-1 + Fn-2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144

The 12th term, F12, is the first term to contain three digits.
What is the first term in the Fibonacci sequence to contain 1000 digits?"""

import time, logging, sys

logging.basicConfig(stream=sys.stdout, level=logging.INFO)

def main():
    for i, fib in enumerate(fib_gen()):
        logging.debug("{}: {}".format(i, fib))
        if len(str(fib)) >= 1000:
            print("Answer: {}".format(i))
            return

def fib_gen():
    a = 0
    b = 1
    while True:
        yield b
        ( a, b ) = ( b, a+b )

if __name__ == "__main__":
    start = time.time()
    main()
    print("That took {:4.2f} seconds".format(time.time() - start))
