#!/usr/bin/env python3

"""
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:
    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
"""

from time import time

def main():
    print('Answer: {}'.format(sum(1 for _ in change_gen(200))))

def change_gen_slow(value):
    coins  = [ 1, 2, 5,10,20,50,100,200 ]
    limits = [ 0 ] * len(coins)
    for i, coin in enumerate(coins):
        limits[i] = value/coin
    change = [ 0 ] * len(coins)
    done = False
    while not done:
        change_value = [a*b for a,b in zip(change, coins)]
        if sum(change_value) == value:
            yield change
        change[0] += 1
        for i in range(len(change)):
            if change[i] > limits[i]:
                change[i] = 0
                if i+1<len(change):
                    change[i+1] += 1
                else:
                    done = True

def change_gen(value):
    coins = [  1,  2,  5, 10, 20, 50,100,200]
    change = make_change(value, coins)
    len_coins = len(coins)
    yield change
    while max( change[1:] ) > 0:
        temp_value = 0
        for i in range(1,len(change)):
            if change[i] > 0:
                change[i] -= 1
                temp_value += coins[i]
                break
        temp_value += change[0]
        change[0] = 0
        small_change = make_change(temp_value, coins[:i], len_coins)
        change = [a+b for a,b in zip(change, small_change)]
        yield change

def make_change(amount, coins, len_change=0):
    i = len(coins)-1
    change = [ 0 ] * max( len(coins), len_change)
    while amount > 0 and i >= 0:
        change[i] = int( amount/coins[i] )
        amount = amount % coins[i]
        i -= 1
    return change

if __name__ =="__main__":
    start = time()
    main()
    print('That took {:.4f} seconds'.format(time() - start))
