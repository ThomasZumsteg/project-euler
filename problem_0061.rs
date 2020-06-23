#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use std::collections::VecDeque;

type PolyFunc = Box<dyn Fn(usize) -> bool>;

struct Frame<'a> {
    elements: Vec<usize>,
    element: usize, 
    tests: &'a Vec<PolyFunc>,
    test: usize,
}

impl <'a> Frame<'a> {
    fn new(elements: Vec<usize>, tests: &'a Vec<PolyFunc>) -> Frame {
        Frame {
            elements: elements,
            element: 0,
            tests: tests,
            test: 0,
        }
    }
}

fn is_poly(p: usize) -> PolyFunc {
    Box::new(move |n: usize| -> bool {
        if let Some(root) = integer_square_root(p * p + 16 + 8 * p * n - 8 * p - 16 * n ) {
            return (root + p - 4) % (2 * p - 4) == 0;
        }
        false
    })
}

struct Chain<'a> {
    stack: VecDeque<Frame<'a>>,
    tests: &'a Vec<PolyFunc>,
}

impl <'a> Chain<'a> {
    fn new(chain: Vec<usize>, tests: &'a Vec<PolyFunc>) -> Chain {
        let mut stack = VecDeque::new();
        stack.push_back(Frame::new(chain, tests));
        Chain {
            stack: stack,
            tests: tests,
        }
    }
}

impl <'a> Iterator for Chain<'a> {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        while self.stack.len() < self.tests.len() {
            unimplemented!();
        }
        Some(self.stack.iter().map(|f| f.elements[f.element]).collect())
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
    let from = 10usize.pow(digits as u32 - 1);
    let to = 10usize.pow(digits as u32);
    let tests: Vec<PolyFunc> = (3..(set_size+3)).map(|n| is_poly(n)).collect();
    let sets: Vec<Vec<usize>> = Chain::new((from..to).collect(), &tests).collect();
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
        let tests = vec![is_poly(3), is_poly(4)];
        let mut chain = Chain::new((10..100).collect(), &tests);
        assert_eq!(chain.next(), Some(vec![12, 21]));
    }
}
