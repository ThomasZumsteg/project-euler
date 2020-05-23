#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use log::info;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 47, https://projecteuler.net/problem=47")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg n_factors: +takes_value "Distinct prime factors")
        (@arg n_times: +takes_value "Number of times that distinct factor occure in series")
    ).get_matches();
    set_log_level(&args);

    let n_factors = args.value_of("n_factors").map(|n| n.parse::<usize>().unwrap()).unwrap_or(4);
    let n_times = args.value_of("n_times").map(|n| n.parse::<usize>().unwrap()).unwrap_or(4);
    info!("{} distinct factors {} times in sequence", n_factors, n_times);

    let mut root = 2;
    let mut primes = Primes::new();
    'outer: loop {
        let mut count = 0;
        loop {
            let factors = primes.prime_factors(root+count);
            info!("{}: {:?}", root+count, factors);
            count += 1;
            if factors.len() != n_factors {
                break;
            }
            if count >= n_times {
                break 'outer;
            }
        }
        root += count;
    }
    println!("{}", root);
}
