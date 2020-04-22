#[macro_use]
extern crate clap;

use common::set_log_level;
use log::info;
use num::BigUint;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 20, https://projecteuler.net/problem=20")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Power") 
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(100);

    let result = (1..limit).fold(BigUint::from(1usize), |acc, n| acc * BigUint::from(n));
    info!("Result: {}", result);

    let total: u32 = result.to_string().chars().map(|n| n.to_digit(10).unwrap()).sum();
    println!("{}", total);
}
