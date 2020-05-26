#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use log::info;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 50, https://projecteuler.net/problem=50")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg limit: +takes_value "Limit of search")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000000);

    let mut primes = Primes::new();
    let mut result: Option<(usize, usize)> = None;
    for start in 0.. {
        let mut total = primes.nth_prime(start);
        if limit < total {
            break;
        }
        for p in (start+1).. {
            total += primes.nth_prime(p);
            if limit < total {
                break;
            }
            if primes.is_prime(total) {
                info!("{} - {}", total, p - start);
                if result.is_none() || result.unwrap().1 < p - start {
                    result = Some((total, p - start));
                }
            }
        }
    }
    println!("{}", result.unwrap().0);
}
