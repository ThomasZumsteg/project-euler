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
    root: Box<Iterator<Item=usize>>,
    replace: usize,
    set_size: usize,
    current: Vec<usize>,
}

impl Chain {
    fn new(digits: usize, replace: usize, set_size: usize) -> Chain {
        let root = Box::new(10usize.pow(digits as u32 - 1)..10usize.pow(digits as u32));
        Chain {
            root: root,
            replace: replace,
            set_size: set_size,
            current: Vec::new(),
        }
    }
}

impl Iterator for Chain {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        while self.current.len() + 1 < self.set_size {
            unimplemented!()
        }
        let pre = self.current[0].to_string()[0..self.replace].to_string();
        let post = self.current.last().unwrap().to_string()[self.replace..].to_string();
        self.current.push((post + &pre).parse::<usize>().unwrap());
        Some(self.current.clone())
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
    let poly_funcs: Vec<Box<dyn Fn(usize) -> bool>> = (3..(set_size+3)).map(|p| is_poly(p)).collect();
    let mut sets: Vec<Vec<usize>> = Vec::new();
    for set in Chain::new(digits, digits / 2, set_size) {
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

    #[test]
    fn test_chain() {
        let mut chain = Chain::new(8128..8129, 2, 3);
        assert_eq!(chain.next(), Some(vec![8128, 2810, 1081]));
        assert_eq!(chain.next(), Some(vec![8128, 2811, 1181]));
        assert_eq!(chain.next(), Some(vec![8128, 2812, 1281]));
    }
}
