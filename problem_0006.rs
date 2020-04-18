#[macro_use]
extern crate clap;

use common::set_log_level;

fn squaresum(limit: usize) -> usize {
    limit * limit * (limit + 1) * (limit + 1) / 4
}

fn sumsquares(limit: usize) -> usize {
    (1..limit+1).map(|i| i * i).sum()
}


fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 5, https://projecteuler.net/problem=5")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Upper limit of palendrome factors") 
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(100);
    println!("{}", squaresum(limit) - sumsquares(limit));
}
