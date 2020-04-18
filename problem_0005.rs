#[macro_use]
extern crate clap;

use log::{trace, debug};
use std::collections::HashMap;
use std::cmp;
use common::set_log_level;

use common::primes::Primes;

fn prime_factors(int: usize) -> Vec<usize> {
    let mut primes = Primes::new();
    let mut result = Vec::new();
    let mut remainer = int;
    debug!("Factoring: {}", int);
    while remainer > 1 {
        let p = primes.next().unwrap();
        trace!("Testing {} % {} = {}", remainer, p, remainer % p);
        while remainer % p == 0 {
            result.push(p);
            debug!("Factor {}: {}", int, p);
            remainer /= p;
        }
    }
    debug!("Factors {}: {:?} ", int, result);
    result
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 5, https://projecteuler.net/problem=5")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Upper limit of palendrome factors") 
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(20);
    let result = (1..limit)
        .fold(HashMap::new(), |mut acc, num| {
            let mut counts = HashMap::new();
            for factor in prime_factors(num) {
                *counts.entry(factor).or_insert(0) += 1;
            }
            for (key, value) in counts {
                acc.insert(key, cmp::max(value, acc.get(&key).unwrap_or(&value).clone()));
            }
            acc
        })
        .iter()
        .fold(1, |mut total, (factor, &count)| {
            for _ in 0..count {
                total *= factor;
            }
            total
        });
    println!("{}", result);
}
