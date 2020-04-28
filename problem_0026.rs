#[macro_use]
extern crate clap;

use common::set_log_level;
use std::collections::HashMap;

struct Fraction {
    remainer: usize,
    divisor: usize,
}

impl Fraction {
    fn new(num: usize, denom: usize) -> Fraction {
        Fraction { remainer: num, divisor: denom }
    }
}

impl Iterator for Fraction {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.remainer == 0 {
            return None;
        }
        let remainer = self.remainer * 10;
        let digit = remainer / self.divisor;
        self.remainer = remainer % self.divisor;
        Some(digit)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 26, https://projecteuler.net/problem=26")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Denominator limit")
    ).get_matches();
    set_log_level(&args);

    let digits = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);
    let result = (1..digits).max_by_key(|&n| {
        let mut history = HashMap::new();
        let mut fraction = Fraction::new(1, n);
        let mut step = 0;
        loop {
            if let Some(digit) = fraction.next() {
                if let Some(prev) = history.get(&(digit, fraction.remainer)) {
                    return step - prev;
                }
                history.insert((digit, fraction.remainer), step);
            } else {
                return step;
            }
            step += 1;
        }
    }).unwrap();
    println!("{}", result);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_one_sixth() {
        let mut frac = Fraction::new(1, 6);
        assert_eq!(frac.next(), Some(1));
        assert_eq!(frac.next(), Some(6));
        assert_eq!(frac.next(), Some(6));
    }

    #[test]
    fn test_one_half() {
        let mut frac = Fraction::new(1, 2);
        assert_eq!(frac.next(), Some(5));
        assert_eq!(frac.next(), None);
    }

    #[test]
    fn test_one_seventh() {
        let mut frac = Fraction::new(1, 7);
        assert_eq!(frac.next(), Some(1));
        assert_eq!(frac.next(), Some(4));
        assert_eq!(frac.next(), Some(2));
        assert_eq!(frac.next(), Some(8));
        assert_eq!(frac.next(), Some(5));
        assert_eq!(frac.next(), Some(7));
    }

    #[test]
    fn test_one_eigth() {
        let mut frac = Fraction::new(1, 8);
        assert_eq!(frac.next(), Some(1));
        assert_eq!(frac.next(), Some(2));
        assert_eq!(frac.next(), Some(5));
        assert_eq!(frac.next(), None);
    }
}
