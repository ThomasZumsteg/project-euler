#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use common::digits::Digits;
use log::{debug, info};
use std::collections::HashSet;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 35, https://projecteuler.net/problem=35")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "limit")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000000);

    let mut primes = Primes::new();
    let mut circular_primes: HashSet<usize> = HashSet::new();

    loop {
        let prime = primes.next().unwrap();
        if prime > limit {
            break;
        }
        if circular_primes.contains(&prime) {
            continue
        }
        let mut digits = Digits::from(prime);
        let mut seen = HashSet::new(); 
        debug!("Testing {}", prime);
        while primes.is_prime(usize::from(digits.clone())) {
            debug!("Circular {:?}", digits);
            if seen.contains(&usize::from(digits.clone())) {
                // Loop!
                info!("Update {:?}", seen);
                circular_primes.extend(&seen);
                break;
            }
            seen.insert(usize::from(digits.clone()));
            digits.digits.rotate_right(1);
        }
    }
    println!("{}", circular_primes.len())
}
