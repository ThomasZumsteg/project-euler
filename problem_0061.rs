#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use log::{debug, info};
use std::collections::VecDeque;

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
    pre: usize,
    post: usize,
    limit: usize,
}

impl Chain {
    fn new(number: usize, replace: u32) -> Chain {
        debug!("{} - {}", number, replace);
        let replaced = 10usize.pow(replace);
        let digits = number.to_string().len();
        Chain {
            pre: (number % replaced) * 10usize.pow(digits as u32 - replace),
            post: 0,
            limit: replaced,
        }
    }
}


impl Iterator for Chain {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.limit <= self.post {
            return None;
        }
        let result = self.pre + self.post;
        self.post += 1;
        Some(result)
    }
}


fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 61, https://projecteuler.net/problem=61")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg set_size: +takes_value "Number of numbers in the set")
    ).get_matches();
    set_log_level(&args);

    let set_size = args.value_of("set_size").map(|n| n.parse::<usize>().unwrap()).unwrap_or(6);
    let poly_funcs: Vec<Box<dyn Fn(usize) -> bool>> = (3..(set_size+3)).map(|p| is_poly(p)).collect();
    let mut set: Vec<usize> = Vec::new();
    let mut stack: VecDeque<Box<dyn Iterator<Item=usize>>> = VecDeque::new();
    stack.push_front(Box::new(1000..10000));
    'outer: loop {
        let mut num = None;
        while num.is_none() {
            if let Some(iter) = stack.front_mut() {
                num = iter.next();
                if let Some(n) = num {
                    stack.push_front(Box::new(Chain::new(n, 2)));
                } else {
                    stack.pop_front();
                }
            } else {
                break 'outer;
            }
        }
        info!("{:?}", num);
    //     let matchs = poly_funcs.iter().enumerate();
    //     if len(matches) == 1 {
    //         set.push((num, matches[0])); 
    //         stack.push(Chain::new(num));
    //     }
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
        let mut chain = Chain::new(8128, 2);
        assert_eq!(chain.next(), Some(2800));
        assert_eq!(chain.next(), Some(2801));
        assert_eq!(chain.next(), Some(2802));
        assert_eq!(chain.next(), Some(2803));
        assert_eq!(chain.next(), Some(2804));
        for _ in 0..(100-5) {
            assert_ne!(chain.next(), None);
        }
        assert_eq!(chain.next(), None);
    }
}
