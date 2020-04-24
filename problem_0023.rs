#[macro_use]
extern crate clap;

use common::{set_log_level, find_divisors};
use std::collections::HashSet;
use log::debug;

struct Abundant(usize);

impl Abundant {
    fn new() -> Abundant {
        Abundant(0)
    }
}

impl Iterator for Abundant {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        loop {
            self.0 += 1;
            let total: usize = find_divisors(self.0).iter().sum::<usize>() - self.0;
            if total > self.0 {
                return Some(self.0);
            }
        }
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 23, https://projecteuler.net/problem=23")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Limit to test to") 
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(28123);
    let abundant = Abundant::new().take_while(|&n| { debug!("{}", n); n <= limit }).collect::<Vec<usize>>();
    let mut results: HashSet<usize> = HashSet::new();
    for i in 0..abundant.len() {
        for j in i..abundant.len() {
            let result = abundant[i] + abundant[j];
            if result > limit {
                break
            }
            results.insert(result);
        }
    }
    let total = limit * (limit + 1) / 2 - results.iter().sum::<usize>();
    println!("{}", total);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_abundant() {
        let mut abundant = Abundant::new();
        assert_eq!(abundant.next(), Some(12)); // 1 2 3 4 6 = 16
        assert_eq!(abundant.next(), Some(18)); // 1 2 3 6 9 = 21
    }
}
