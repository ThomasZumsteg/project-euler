#!/usr/bin/python
"""Finds the number of ways to express 1/2 as a sum of inverse squares using distinct integers between 2 and 80 inclusive"""

import math

from time import time
from sys import stdout
from fractions import Fraction
from itertools import combinations, count
from collections import namedtuple


def main():
    sets = tree_for_factors([2,2,2,2,3,3,5,5,7,7,13,17])
    return len(sets)

def sets_for_factors(factors, limit = 80):
    integers = []
    seen = set()
    for l in range(len(factors)):
        for combo in combinations(factors, l + 1):
            value = product(combo)
            if value in seen or limit < value:
                continue
            seen.add(value)
            integers.append((
                product(inverse(factors, combo))**2,
                value,
                combo))
    integers.sort(key=lambda e: e[1])
    target = product(factors) ** 2 // 2
    count = 0;
    for l in range(len(integers)):
        for combo in combinations(integers, l + 1):
            if sum(c[0] for c in combo) == target:
                count += 1
                print('{:02}: {{{}}}'.format(count, ','.join(str(c[1]) for c in combo)))

State = namedtuple("State", ("tree", "factors", "remaining"))

def lcm(a, b):
    cf = math.gcd(a, b)
    return a * b / cf

def tree_for_factors(factors, limit=80):
    tree = []
    least_common_multiple = product(factors) ** 2
    for n in range(2, limit+1):
        divisors = factors.copy()
        remainer = n
        while divisors:
            d = divisors.pop()
            if remainer % d == 0:
                remainer /= d
            if remainer == 1:
                tree.append((n, least_common_multiple // (n**2)))
                break
    queue = [State(tree, (), least_common_multiple / 2)]
        
    seen = set()
    while queue:
        state = queue.pop()
        if state.remaining == 0:
            assert state.factors not in seen
            seen.add(state.factors)
            print("{:3}: {}".format(len(seen), state.factors))
        elif state.remaining > 0 and state.tree and\
            state.remaining <= sum(t[1] for t in state.tree):
            queue.append(State(state.tree[1:], state.factors, state.remaining))
            queue.append(State(state.tree[1:], state.factors + (state.tree[0][0],), 
                state.remaining - state.tree[0][1]))
    return seen

def inverse(all_factors, factors):
    result = list(all_factors[:])
    for f in factors:
        result.remove(f)
    return tuple(result)

def product(items):
    result = 1
    for item in items:
        result *= item
    return result

def find_group_by_itteration(lim):
    queue = [(Fraction(1,2), [], 1)]
    sums_list = get_sums(lim)
    while queue:
        remainer, group, position = queue.pop()
        #stdout.write("%6f: %3d: %3d\r" %(remainer, position, len(queue)))
        #stdout.flush()
        for n in range(position, lim+1):
            stdout.write("%-60s" %(group[-10:] + [n]))
            new_fraction = Fraction(1, n**2)
            if new_fraction <= remainer and len(group) < 12:
                (new_remainer, new_group) = (remainer - new_fraction, group + [n])
                if remainer > sums_list[n]:
                    (remainer, group) = (new_remainer, new_group)
                    stdout.write("  Required\r")
                else:
                    queue.append((new_remainer, new_group, n+1))
                    stdout.write("Skipped\r")
            else: stdout.write("  Dropped\r")
        if remainer == 0:
            stdout.write(" " * 80 + "\r")
            print("{}".format(group))

def valid_group(group, current, limit):
    paired_factors = {}
    unpaired_factors = {}
    for item in group:
        for factor, power in get_factors(item):
            if factor not in paired_factors or power > paired_factors[factor]:
                if factor in unpaired_factors:
                    if power == unpaired_factors[factor]:
                        unpaired_factors.pop(factor)
                        paired_factors[factor] = power
                    else:
                        unpaired_factors[factor] = max(power, unpaired_factors[factor])
                else:
                    unpaired_factors[factor] = power
    #print paired_factors
    #print unpaired_factors
    for factor, power in unpaired_factors.iteritems():
        if current // (factor**power) >= limit // (factor**power):
            print(factor)
            return False
    return True

def get_factors(item):
    for p in prime_generator():
        if item < p: break
        n = 1
        while item % p**n == 0:
            n += 1
        if n > 1: yield (p,n-1)

def get_sums(lim):
    convergent_sums = [ None ] * (lim + 1)
    f_sum = Fraction(0, 1)
    for i in reversed(range(1,lim + 1)):
        convergent_sums[i] = f_sum
        f_sum += Fraction(1, i**2)
    return convergent_sums


def find_group_sequentially(lim):
    """Finds valid groups by squentially updating potential groups from 1 to [lim]""" 
    # Uses too much memory
    out_queue = [(Fraction(1,2), [])]
    for n in range(1,lim+1):
        convergent_sum = sum([Fraction(1,x**2) for x in range(n+1,lim+1)], Fraction(0,1))
        print("{:2d}: {9d}".format(n, len(out_queue)))
        in_queue = out_queue
        out_queue = []
        n_fraction = Fraction(1,n**2)
        for (remainer, group) in in_queue:
            #print_this = False
            #if group == None:
            #    print_this = True
            #if print_this: print "%6f: %-30s" %(remainer, str(group + [n])),
            if n_fraction <= remainer:
                out_queue.append((remainer - n_fraction, group[:] + [n]))
            #    if print_this: print "Added",
            #elif print_this: print "     ",
            if convergent_sum >= remainer:
                out_queue.append((remainer, group[:]))
            #    if print_this: print "Skipped",
            #if print_this: print ""
    return out_queue

def find_group_dec(lim):
    limits = get_limits_dec(lim)
    memory = [(1, 1.0/2, [])]
    i = 0
    while memory:
        (start, remainer, group) = memory.pop()
        print("Got {:2d}".format(start))
        for n in range(start+1,lim+1):
            if i > 200:
                print("EXIT!")
                return 0
            i += 1
            sum_remaining = limits[n]
            print("{:2d}: {: 6f}: {: 6f}".format(n, remainer - 1.0 / n**2, remainer - sum_remaining)),
            if 1.0 / n**2 > remainer: 
                print("Too Big")
                continue
            elif sum_remaining > remainer:
                memory.append((n, remainer - 1.0 / n**2, group[:] + [n]))
                print("Saved")
            else:
                print("Required")
                remainer -= 1.0 / n**2
                group.append(n)
        if remainer == 0: print(group)
    return 0

def show_me(g):
    print(g)
    (n,d) = (0,1)
    for e in sorted(g):
        (n, d) = (n * e**2 + d, d * e**2)
        f = gcd(n,d)
        (n,d) = (n/f, d/f)
        print("{:2d}: {:10d} / {:10d}".format(e, n,d))

def gcd(a,b):
    """Finds the greatest common factor using Euclid's algorithm. See http://en.wikipedia.org/wiki/Euclid%27s_algorithm"""
    while b != 0: (a,b) = (b, a % b)
    return a

def lcm(group):
    common_factor = 1
    for number in group:
        common_factor = (common_factor * number) / gcd(common_factor, number)
    return common_factor

#def get_limits_dec(lim):
#    limits = [0] * (lim + 1)
#    for i in reversed(range(3,lim+1)):
#        limits[i-1] = limits[i] + 1.0 / i**2
#    return limits

#def get_limits_frac(lim):
#    limits = [(0,1)] * (lim+1)
#    for i in reversed(range(3,lim+1)):
#        (n,d) = limits[i]
#        limits[i-1] = (n * i**2 + d, d * i**2)
#    return limits

if __name__ == "__main__":
    start = time()
    print("Answer: {d}".format(main()))
    print("That took {f} seconds".format(time() - start))
