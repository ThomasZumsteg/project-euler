#[macro_use]
extern crate clap;

use common::set_log_level;
use common::digits::Digits;
use num::bigint::BigInt;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 57, https://projecteuler.net/problem=57")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg expansions: +takes_value "Number of expansions to perform")
    ).get_matches();
    set_log_level(&args);

    let expansions = args.value_of("expansions").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);

    let mut total = 0;
    let mut num = BigInt::from(1);
    let mut den = BigInt::from(2);
    for _ in 0..expansions {
        let new_num = 2 * den.clone() + num;
        num = den;
        den = new_num;
        if Digits::from(num.clone() + den.clone()).digits.len() > Digits::from(den.clone()).digits.len() {
            total += 1;
        }
    }
    println!("{}", total);
}
