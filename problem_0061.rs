#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use log::{debug, info};
use std::collections::{HashSet, VecDeque};

fn is_poly(p: usize) -> Box<dyn Fn(usize) -> bool> {
    let poly = Box::new(move |m: usize| -> bool {
        if let Some(root) = integer_square_root(p * p + 16 + 8 * p * m - 8 * p - 16 * m ) {
            return (root + p - 4) % (2 * p - 4) == 0;
        }
        false
    });
    poly
}

struct Chain {
    tests: Vec<Box<dyn Fn(usize)->bool>>,
    chain: Box<dyn Iterator<Item=usize>>,
    digits: usize,
    current: usize,
    next_chain: Option<Box<Chain>>,
}

impl Chain {
    fn new(digits: usize, chain: Box<dyn Iterator<Item=usize>>, tests: Vec<Box<dyn Fn(usize)->bool>>) -> Chain {
        let mut chain = Chain {
            tests: tests,
            chain: chain,
            digits: digits,
            current: 0,
            next_chain: None,
        };
        chain.next_chain = chain.step();
        chain
    }

    fn step(&mut self) -> Option<Box<Chain>> {
        loop {
            if let Some(current) = self.chain.next() {
                self.current = current;
                let prefix = current.to_string()[self.digits/2..].to_string();
                let from = (prefix.clone() + &"0".repeat(self.digits/2)).parse::<usize>().unwrap();
                let to = (prefix.clone() + &"9".repeat(self.digits/2)).parse::<usize>().unwrap() + 1;
                unimplemented!()
            } else {
                return None;
            }
        }
    }
}

impl Iterator for Chain {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
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
    let tests: Vec<Box<dyn Fn(usize) -> bool>> = (3..(set_size+3)).map(|p| is_poly(p)).collect();
    let mut sets: Vec<Vec<usize>> = Vec::new();
    let from = 10usize.pow(digits as u32 - 1);
    let to = 10usize.pow(digits as u32);
    for set in Chain::new(digits, Box::new(from..to), tests) {
        info!("{:?}", set);
    }
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
        assert!(is_poly(3)(10));

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
        assert!(is_poly(5)(12));

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
}
