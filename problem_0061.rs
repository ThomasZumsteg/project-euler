#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use log::{debug, info};
use std::collections::{HashSet, VecDeque};

fn is_poly(p: usize) -> Box<dyn Fn(usize) -> bool> {
    Box::new(move |n: usize| -> bool {
        if let Some(root) = integer_square_root(p * p + 16 + 8 * p * n - 8 * p - 16 * n ) {
            return (root + p - 4) % (2 * p - 4) == 0;
        }
        false
    })
}

struct Chain {
    chain: Box<dyn Iterator<Item=usize>>,
    next_chain: Option<Box<Chain>>,
}

impl Chain {
    fn new(digits: usize, chain: Box<dyn Iterator<Item=usize>>, tests: VecDeque<Box<dyn Fn(usize) -> bool>>) -> Option<Chain> {
        if tests.len() <= 0 {
            return None;
        }
        Some(Chain {
            chain: chain,
            next_chain: None,
        })
    }
}

impl Iterator for Chain {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        if let Some(next_chain) = &mut self.next_chain {
        } else {
            unimplemented!()
        }
        unimplemented!()
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 61, https://projecteuler.net/problem=61")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg set_size: +takes_value "Size of the set")
        (@arg digits: +takes_value "Digits in each number")
        (@arg replace: +takes_value "Number of digits in common")
    ).get_matches();
    set_log_level(&args);

    let set_size = args.value_of("set_size").map(|n| n.parse::<usize>().unwrap()).unwrap_or(6);
    let digits = args.value_of("digits").map(|n| n.parse::<usize>().unwrap()).unwrap_or(4);
    let tests: VecDeque<Box<dyn Fn(usize) -> bool>> = (3..(set_size+3)).map(|n| is_poly(n)).collect();
    let from = 10usize.pow(digits as u32 - 1);
    let to = 10usize.pow(digits as u32);
    let sets: Vec<Vec<usize>> = Chain::new(digits, Box::new(from..to), tests).unwrap().collect();
    assert_eq!(sets.len(), 1);
    println!("{}", sets[0].iter().sum::<usize>());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_is_poly() {
        assert!(is_poly(3)(1));
        assert!(!is_poly(3)(2));
        assert!(is_poly(3)(3));
        assert!(!is_poly(3)(4));
        assert!(!is_poly(3)(5));
        assert!(is_poly(3)(6));
        assert!(!is_poly(3)(7));
        assert!(is_poly(3)(1));

        assert!(is_poly(4)(1));
        assert!(!is_poly(4)(2));
        assert!(!is_poly(4)(3));
        assert!(is_poly(4)(4));
        assert!(is_poly(4)(9));

        assert!(is_poly(5)(1));
        assert!(!is_poly(5)(2));
        assert!(!is_poly(5)(3));
        assert!(!is_poly(5)(4));
        assert!(is_poly(5)(5));
        assert!(is_poly(5)(1));

        assert!(is_poly(6)(1));
        assert!(!is_poly(6)(2));
        assert!(!is_poly(6)(3));
        assert!(!is_poly(6)(4));
        assert!(!is_poly(6)(5));
        assert!(is_poly(6)(6));
        assert!(is_poly(6)(15));

        assert!(is_poly(7)(1));
        assert!(!is_poly(7)(2));
        assert!(!is_poly(7)(6));
        assert!(is_poly(7)(7));
        assert!(is_poly(7)(18));

        assert!(is_poly(8)(1));
        assert!(!is_poly(8)(2));
        assert!(!is_poly(8)(7));
        assert!(is_poly(8)(8));
        assert!(is_poly(8)(21));
    }

    #[test]
    fn test_chain() {
        let mut chain = Chain::new(2, Box::new(10..100), vec![is_poly(3), is_poly(4)].iter().collect());
        assert_eq!(chain.next(), Some(vec![12, 21]));
    }
}
