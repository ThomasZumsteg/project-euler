#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 5, https://projecteuler.net/problem=5")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg nth: "Get the nth prime number") 
    ).get_matches();
    set_log_level(&args);

    let nth = args.value_of("nth").map(|n| n.parse::<usize>().unwrap()).unwrap_or(10001-1);
    println!("{}", Primes::new().nth(nth).unwrap());
}
