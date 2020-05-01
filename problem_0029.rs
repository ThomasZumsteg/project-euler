#[macro_use]
extern crate clap;

use common::set_log_level;
use std::collections::HashSet;
use num::BigInt;
use num::pow::pow;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 29, https://projecteuler.net/problem=29")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "limit")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(100);

    let mut result: HashSet<BigInt> = HashSet::new();
    for a in 2..(limit+1) {
        for b in 2..(limit+1) {
            result.insert(pow(BigInt::from(a), b));
        }
    }
    println!("{}", result.len());
}
