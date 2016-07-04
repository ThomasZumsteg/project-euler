#!/usr/bin/python
"""Finds the number of ways to express 1/2 as a sum of inverse squares using distinct integers between 2 and 80 inclusive"""

from time import time
from sys import stdout
from itertools import combinations, count
from common import prime_generator

def main():
    find_group_sequentially(35)
    return 0

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
            print "%s" %(group)

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
            print factor
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

class Fraction(object):
    def __init__(self, numerator, denominator):
        self.numerator = numerator
        self.denominator = denominator

    def __add__(self, f):
        numerator = self.numerator * f.denominator + self.denominator * f.numerator
        denominator = self.denominator * f.denominator
        common_factor = self.gcd(numerator, denominator)
        return Fraction(numerator / common_factor, denominator / common_factor)

    def __sub__(self, f):
        numerator = self.numerator * f.denominator - self.denominator * f.numerator
        denominator = self.denominator * f.denominator
        common_factor = self.gcd(numerator, denominator)
        return Fraction(numerator / common_factor, denominator / common_factor)

    def __neg__(self):
        return Fraction(-self.numerator, self.denominator)

    def __float__(self):
        return float(self.numerator) / self.denominator

    def __str__(self):
        return "%d/%d" %(self.numerator, self.denominator)

    def __repr__(self):
        return self.__str__()

    def __lt__(self, f):
        return self.numerator * f.denominator < self.denominator * f.numerator

    def __le__(self, f):
        return self.numerator * f.denominator <= self.denominator * f.numerator

    def __eq__(self, f):
        return self.denominator * f.numerator == self.numerator * f.denominator

    def __gt__(self, f):
        return self.numerator * f.denominator > self.denominator * f.numerator

    def __ge__(self, f):
        return self.numerator * f.denominator >= self.denominator * f.numerator

    def gcd(self, a, b):
        """Finds the greatest common factor using Euclid's algorithm. See http://en.wikipedia.org/  wiki/Euclid%27s_algorithm"""
        while b != 0: (a,b) = (b, a % b)
        return a

def find_group_sequentially(lim):
    """Finds valid groups by squentially updating potential groups from 1 to [lim]""" 
    # Uses too much memory
    out_queue = [(Fraction(1,2), [])]
    for n in range(1,lim+1):
        convergent_sum = sum([Fraction(1,x**2) for x in range(n+1,lim+1)], Fraction(0,1))
        print "%2d: %9d" %(n, len(out_queue))
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
        print "Got %2d" %(start)
        for n in range(start+1,lim+1):
            if i > 200:
                print "EXIT!"
                return 0
            i += 1
            sum_remaining = limits[n]
            print "%2d: % 6f: % 6f" %(n, remainer - 1.0 / n**2, remainer - sum_remaining),
            if 1.0 / n**2 > remainer: 
                print "Too Big"
                continue
            elif sum_remaining > remainer:
                memory.append((n, remainer - 1.0 / n**2, group[:] + [n]))
                print "Saved"
            else:
                print "Required"
                remainer -= 1.0 / n**2
                group.append(n)
        if remainer == 0: print group
    return 0

def show_me(g):
    print g
    (n,d) = (0,1)
    for e in sorted(g):
        (n, d) = (n * e**2 + d, d * e**2)
        f = gcd(n,d)
        (n,d) = (n/f, d/f)
        print "%2d: %10d / %10d" %(e, n,d)

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
    print "Answer: %d" %(main())
    print "That took %f seconds" %(time() - start)