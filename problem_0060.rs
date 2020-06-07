#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use std::cmp::Ordering;
use std::collections::BinaryHeap;

struct OrderPrimeSet {
    set: Vec<usize>,
    primes: Vec<usize>,
}

impl Ord for OrderPrimeSet {
    fn cmp(&self, other: &Self) -> Ordering {
        other.set.iter().sum::<usize>().cmp(
            &self.set.iter().sum::<usize>()
        )
    }
}

impl PartialOrd for OrderPrimeSet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for OrderPrimeSet {
    fn eq(&self, other: &Self) -> bool {
        self.set.iter().sum::<usize>() == other.set.iter().sum::<usize>()
    }
}

impl Eq for OrderPrimeSet {
}

struct OrderedPrimeSets {
    set_size: BinaryHeap<OrderPrimeSet>,
    primes: Primes,
}

impl OrderedPrimeSets {
    fn new(set_size: usize) -> OrderedPrimeSets {
        let mut heap = BinaryHeap::new();
        OrderedPrimeSets {
            primes: Primes::new(),
            set_size: set_size
        }
    }
}

impl Iterator for OrderedPrimeSets {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        unimplemented!();
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 58, https://projecteuler.net/problem=58")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg set_size: +takes_value "Number of primes to take")
    ).get_matches();
    set_log_level(&args);
    
    let set_size = args.value_of("set_size").map(|n| n.parse::<usize>().unwrap()).unwrap_or(5);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_single_iterator() {
        let iterator = OrderedPrimeSet::new(1);
        assert_eq!(iterator.next(), Some(2));
        assert_eq!(iterator.next(), Some(3));
        assert_eq!(iterator.next(), Some(5));
        assert_eq!(iterator.next(), Some(7));
        assert_eq!(iterator.next(), Some(11));
        assert_eq!(iterator.next(), Some(13));
    }

    #[test]
    fn test_double_iterator() {
        let iterator = OrderedPrimeSet::new(2);
        assert_eq!(iterator.next(), Some(vec![2, 3])); // 5
        assert_eq!(iterator.next(), Some(vec![2, 5])); // 7
        assert_eq!(iterator.next(), Some(vec![3, 5])); // 8
        assert_eq!(iterator.next(), Some(vec![2, 7])); // 9
        assert_eq!(iterator.next(), Some(vec![3, 7])); // 10
    }

    #[test]
    fn test_triple_iterator() {
        let iterator = OrderedPrimeSet::new(2);
        assert_eq!(iterator.next(), Some(vec![2, 3, 5])); // 10
        assert_eq!(iterator.next(), Some(vec![2, 3, 7])); // 12
        assert_eq!(iterator.next(), Some(vec![2, 5, 7])); // 14
        assert_eq!(iterator.next(), Some(vec![3, 5, 7])); // 15
    }
}
