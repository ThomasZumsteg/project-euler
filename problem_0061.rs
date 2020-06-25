#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use std::collections::{HashSet, VecDeque};
use log::{debug, info};

type PolyFunc = Box<dyn Fn(usize) -> bool>;

struct Frame<'a> {
    elements: Vec<usize>,
    element: usize, 
    tests: Vec<&'a PolyFunc>,
    test: usize,
}

impl <'a> Frame<'a> {
    fn new(elements: Vec<usize>, tests: Vec<&'a PolyFunc>) -> Frame {
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
    digits: usize,
}

impl <'a> Chain<'a> {
    fn new(chain: Vec<usize>, tests: Vec<&'a PolyFunc>, digits: usize) -> Chain {
        let mut stack = VecDeque::new();
        stack.push_back(Frame::new(chain, tests.clone()));
        debug!("{}@{}", tests.len(), digits);
        Chain {
            stack: stack,
            digits: digits,
        }
    }
}

impl <'a> Iterator for Chain<'a> {
    type Item = Vec<usize>;

    fn next(&mut self) -> Option<Vec<usize>> {
        while self.stack.len() > 0 {
            if self.stack.back().unwrap().tests.len() <= 0 {
                let result: Vec<usize> = self.stack.iter().take(self.stack.len()-1).map(|f| f.elements[f.element]).collect();
                self.stack.pop_back();
                let first = result[0].to_string();
                let last = result[result.len()-1].to_string();
                if first[..(self.digits/2)] == last[(self.digits/2)..] {
                    info!("{:?}", result);
                    return Some(result);
                }
            }
            let stack_len = self.stack.len();
            if let Some(frame) = self.stack.back_mut() {
                if frame.test < frame.tests.len() {
                    let test = &frame.tests[frame.test];
                    frame.test += 1;
                    let element = frame.elements[frame.element];
                    debug!("Stack: {}, Index {}, element: {}, test index: {}", stack_len, frame.element, element, frame.test);
                    if test(element) {
                        let mut tests: Vec<&PolyFunc> = frame.tests.clone();
                        debug!("Element {} passed test {}/{}", element, frame.test, tests.len());
                        tests.remove(frame.test-1);
                        let prefix = element.to_string()[(self.digits/2)..].to_string();
                        if &prefix[0..1] == "0" {
                            continue;
                        }
                        let start = (prefix.clone() + &"0".repeat(self.digits/2)).parse::<usize>().unwrap();
                        let end = (prefix.clone() + &"9".repeat(self.digits/2)).parse::<usize>().unwrap();
                        let next_frame = Frame::new(
                            (start..(end+1)).collect(),
                            tests,
                        );
                        self.stack.push_back(next_frame);
                        continue;
                    }
                } else {
                    frame.element +=  1;
                    frame.test = 0;
                    if frame.elements.len() <= frame.element {
                        self.stack.pop_back();
                    } 
                }
            } else {
                return None;
            }
        }
        None
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
    let result: HashSet<usize> = Chain::new((from..to).collect(), tests.iter().collect(), digits)
        .map(|set| set.iter().sum()).collect();
    assert_eq!(result.len(), 1);
    println!("{}", result.iter().next().unwrap());
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
        let mut chain = Chain::new((10..100).collect(), tests.iter().collect(), 2);
        assert_eq!(chain.next(), Some(vec![12, 21]));
    }
}
