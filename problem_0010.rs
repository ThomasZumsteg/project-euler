#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 10, https://projecteuler.net/problem=10")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Limit of primes to sum") 
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(2000000);
    println!("{}", Primes::new().take_while(|&x| x < limit).sum::<usize>());
}
