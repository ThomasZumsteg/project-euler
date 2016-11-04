#!/usr/bin/env python3
"""
Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
1634 = 1^4 + 6^4 + 3^4 + 4^4
8208 = 8^4 + 2^4 + 0^4 + 8^4
9474 = 9^4 + 4^4 + 7^4 + 4^4
As 1 = 1^4 is not a sum it is not included.
The sum of these numbers is 1634 + 8208 + 9474 = 19316.
Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
"""

from time import time

def main():
    fifth_power_sum = 0
    for i in range(2,360000):
        temp = [int(x)**5 for x in list(str(i))]
        if i == sum(temp):
            fifth_power_sum += i
    print('Answer:{}'.format(fifth_power_sum))

if __name__ == "__main__":
    start = time()
    main()
    print('That took {:.4f} seconds'.format(time() - start))
