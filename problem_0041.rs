#[macro_use]
extern crate clap;

use common::set_log_level;
use common::digits::Digits;
use common::primes::Primes;
use itertools::Itertools;
use log::info;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 41, https://projecteuler.net/problem=41")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg limit: +takes_value "Limits")
    ).get_matches();
    set_log_level(&args);
    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(9);
    
    let mut primes = Primes::new();

    'outer: for len in (1..(limit+1)).rev()  {
        let digits = (1..(len+1)).rev();
        for ordered in digits.permutations(len) {
            let value = usize::from(Digits::from(ordered));
            info!("{}: {}", len, value);
            if primes.is_prime(value) {
                println!("{}", value);
                break 'outer;
            }
        }
    }
}
