#!/usr/bin/env python3


"""
The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
"""

import logging
import sys
import time

from common import prime_generator

logging.basicConfig(
    stream=sys.stderr,
    level=logging.INFO,
    format='%(levelname)s: %(message)s')

def main():
    set_dict = {}
    limit = False
    max_elements = [1e6] * 5
    i = 0
    for prime in prime_generator(block=1000000):
        i += 1
        if i % 100000 == 0: logging.info("Testing {}".format(prime))
        if limit and limit < prime: break
        [p,q] = populate(set_dict, prime)
        logging.debug(set_dict)
        logging.debug("p: {}\nq: {}".format(str(p), str(q)))
        element_set = has_valid_set(set_dict,[p,q])
        if element_set and sum(element_set) < sum(max_elements):
            element_set.sort()
            print("{:d}: {!s}".format(sum(element_set), element_set))
            k = str(element_set[-1])
            limit = int(k + k)
            max_elements = element_set

def populate(set_dict, prime):
    set_dict[prime] = set()
    (p_set, q_set) = (set(), set())
    for i in range(1,len(str(prime))):
        [p,q] = [str(prime)[:i],str(prime)[i:]]
        logging.debug("Adding {!s} and {!s}".format(p,q))
        if q[0] == '0':
            logging.debug("Zero dropout: {}".format(q))
            continue
        elif int(q + p) not in set_dict:
            logging.debug("Not in set_dict: {}".format(q+p))
            continue
        elif int(p) in set_dict and int(q) in set_dict:
            [p,q] = [int(p),int(q)]
            p_set.add(p)
            q_set.add(q)
            set_dict[p].add(q)
            set_dict[q].add(p)
    return [iter(p_set), iter(q_set)]

def has_valid_set(set_dict, canidates):
    c = 0
    elements = []
    while len(elements) < 5:
        logging.debug("elements: {!s}".format(elements))
        try:
            logging.debug("Trying {}".format(c))
            canidate = next(canidates[c])
        except IndexError:
            logging.debug("IndexError caught")
            common_set = set_dict[elements[0]]
            for k in range(1,len(elements)):
                common_set = common_set & set_dict[elements[k]]
            canidates.append(iter(common_set))
        except StopIteration:
            logging.debug("Iterator {} stopped".format(c))
            c -= 1
            if c < 0: return False
        else:
            logging.debug("Adding set_dict[{}] = {}".format(canidate,
                len(set_dict[canidate])))
            if set_dict[canidate] >= set(elements):
                elements.append(canidate)
                c += 1
    return elements

if __name__ == "__main__":
    start = time.time()
    main()
    logging.info("That took {:4.2f} seconds".format(time.time() - start))
