#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use common::primes::Primes;
use log::{info, debug};

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 46, https://projecteuler.net/problem=46")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);

    let mut primes = Primes::new();
    let mut result: Option<usize> = None;
    let mut num = 1;
    while result.is_none() {
        num += 2;
        if primes.is_prime(num) {
            continue;
        }
        for p in 1.. {
            let prime = primes.nth_prime(p);
            debug!("Testing {}: {}", num, prime);
            if num < prime {
                result = Some(num);
                break;
            }
            if let Some(root) = integer_square_root((num - prime) / 2) {
                info!("{} = {} + 2 * {}", num, prime, root);
                break;
            }
        }
    }
    println!("{}", result.unwrap());
}
