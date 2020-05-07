#[macro_use]
extern crate clap;

use common::set_log_level;
use itertools::Itertools;
use log::{debug, info};
use std::collections::HashSet;

fn to_usize(digits: &[usize]) -> usize {
    let mut result = 0;
    for d in digits {
        result = result * 10 + d;
    }
    result
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 32, https://projecteuler.net/problem=32")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "Threads")
    ).get_matches();
    set_log_level(&args);

    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);

    let mut products = HashSet::new();
    for perm in (1..10).permutations(9) {
        for i in 1..8 {
            for j in i+1..9 {
                debug!("{} * {} = {}", to_usize(&perm[0..i]), to_usize(&perm[i..j]), to_usize(&perm[j..9]));
                if to_usize(&perm[0..i]) * to_usize(&perm[i..j]) == to_usize(&perm[j..9]) {
                    info!("{} * {} = {}", to_usize(&perm[0..i]), to_usize(&perm[i..j]), to_usize(&perm[j..9]));
                    products.insert(to_usize(&perm[j..9]));
                }
            }
        }
    }
    println!("{}", products.iter().sum::<usize>());
}
