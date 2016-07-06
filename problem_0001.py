#!/usr/bin/env python3

import argparse, logging, concurrent.futures, sys, time, unittest

def sum_of_multiples(limit, mutliples):
    total = 0
    for n in range(1, limit):
        if any(n % m == 0 for m in mutliples):
            total += n
    return total

class TestSumOfMultiples(unittest.TestCase):
    def test_base_case(self):
        self.assertEqual(sum_of_multiples(10, [3, 5]), 23)

    def test_incorrect_result(self):
        self.assertEqual(sum_of_multiples(10, [3, 5, 7]), 30)

    def test_empty_list(self):
        self.assertEqual(sum_of_multiples(10, []), 0)

    def test_bigger_limit(self):
        self.assertEqual(sum_of_multiples(100, [3, 5]), 2318)

class TestTimeSumOfMultiples(unittest.TestCase):
    def test_1000(self):
        try:
            result, time = timeit(sum_of_multiples, 10, [3,5], timeout=10)
        except TimeoutError as ex:
            self.fail(msg='timed out after 10 seconds: {}'.format(ex))
        self.assertEqual(result, 23)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Finds the sum of natural numbers below a limit that are multiples of at least one of a list of integers')
    parser.add_argument('limit',
                        default=1000,
                        type=int,
                        help='hightest integer to include',
                       )
    parser.add_argument('mutliples',
                        default=[3,5],
                        type=int,
                        nargs=argparse.REMAINDER,
                        help='Integer that evenly divide elements to be summed',
                        metavar='multiple',
                       )
    parser.add_argument('--test',
                       action='store_true',
                       help='run the test suite',
                       )
    parser.add_argument('-v', '--verbose',
                        action='store_true',
                        help='show debug messages'
                       )
    parser.add_argument('--time',
                        action='store_true',
                        help='display the running time'
                       )
    parser.add_argument('--timeout',
                        type=float,
                        default=None,
                        help='timout after T seconds',
                        metavar='T',
                       )
    args = parser.parse_args()
    if args.test:
        testing_args = [arg for arg in sys.argv[1:] if arg is not '--test']
        unittest.main(argv=testing_args)
    elif args.timeout and 0 > args.timeout:
        parser.print_help()

    executor = concurrent.futures.ThreadPoolExecutor()
    start = time.time()
    f = executor.submit(sum_of_multiples, args.limit, args.mutliples)
    while not f.done():
        elapsed = time.time() - start
        if args.timeout and elapsed > args.timeout:
            print('\nTimeout')
            f.cancel()
            break
        if args.time:
            sys.stdout.write('\r{:5.2f}'.format(elapsed))
    else:
        print('\n{}'.format(f.result()))
