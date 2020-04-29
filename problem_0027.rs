#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use log::{info, debug};
use std::collections::HashSet;

struct QuadraticPrimes {
    n: usize,
    a: isize,
    b: isize,
}

impl Iterator for QuadraticPrimes {
    type Item = isize;

    fn next(&mut self) -> Option<isize> {
        let result = (self.n * self.n) as isize + self.a * self.n as isize + self.b;
        self.n += 1;
        Some(result)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 27, https://projecteuler.net/problem=27")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Limit")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<isize>().unwrap()).unwrap_or(1000);
    let mut max: Option<(isize, isize)> = None;
    let mut prime_gen = Primes::new();
    let mut primes = HashSet::new();
    for a in -limit..limit {
        for b in -limit..limit {
            let mut n = 0;
            let mut result;
            loop {
                result = n * n + a * n + b;
                debug!("{} * {} + {} * {} + {} = {}", n, n, a, n, b, result);
                while prime_gen.current < result.abs() as usize {
                    primes.insert(prime_gen.next().unwrap());
                }
                if !primes.contains(&(result.abs() as usize)) {
                    break
                }
                n += 1;
            }
            if max.is_none() || max.unwrap().0 < n {
                info!("Max: {}, Value: ({}, {})", n, a, b);
                max = Some((n, a * b));
            }
        }
    }
    println!("{}", max.unwrap().1);
}
