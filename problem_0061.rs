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
    digits: usize,
    set_size: usize,
    current: usize,
}

impl Chain {
    fn new(digits: usize, set_size: usize) -> Chain {
        let current = 10usize.pow(((set_size * digits / 2) - 1) as u32);
        info!("Initial: {}", current);
        Chain {
            digits: digits,
            set_size: set_size,
            current: current,
        }
    }
}

impl Iterator for Chain {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        'search: loop {
            let digits = self.current.to_string();
            if (self.digits * self.set_size / 2) != digits.len() {
                return None;
            }
            info!("Digits: {}", digits);
            self.current += 1;
            let mut result = Vec::new();
            for step in (0..((self.set_size-1) * self.digits / 2)).step_by(self.digits/2) {
                let element = digits[step..(step+self.digits)].to_string();
                if &element[0..1] == "0" {
                    let next = digits.chars().enumerate().map(
                        |(e, d)| if e < step { d } else if e == step { '1' } else { '0' }

                    ).collect::<String>();
                    self.current = next.parse::<usize>().unwrap();
                    continue 'search;
                }
                result.push(element.parse::<usize>().unwrap());
            }
            let pre = digits[..(self.digits / 2)].to_string();
            let post = digits[(digits.len()-(self.digits/2))..].to_string();
            if &pre[0..1] == "0" || &post[0..1] == "0" {
                continue;
            }
            result.push((post + &pre).parse::<usize>().unwrap());
            return Some(result);
        }
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
    for set in Chain::new(digits, set_size) {
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
        let mut chain = Chain::new(4, 3);
        assert_eq!(chain.next(), Some(vec![1010, 1010, 1010]));
        assert_eq!(chain.next(), Some(vec![1010, 1011, 1110]));
        assert_eq!(chain.next(), Some(vec![1010, 1012, 1210]));
        assert_eq!(chain.next(), Some(vec![1010, 1013, 1310]));
        for set in chain {
            assert_eq!(set.len(), 3);
            assert!(set.iter().all(|n| n.to_string().len() == 4));
            for (a, b) in set.iter().zip([&set[1..], &vec![set[0]]].concat().iter()) {
                assert_eq!(a.to_string()[2..], b.to_string()[..2], "{:?}", set);
            }
        }
    }
}
