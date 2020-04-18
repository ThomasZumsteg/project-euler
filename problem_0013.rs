#[macro_use]
extern crate clap;

use common::set_log_level;
use std::fs::File;
use std::io::{BufRead, BufReader};
use num::BigInt;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 13, https://projecteuler.net/problem=13")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg file: "Digits file") 
    ).get_matches();

    set_log_level(&args);

    let file_name = args.value_of("file").unwrap_or("problem_0013.txt");
    let file = File::open(file_name).unwrap();

    let mut sum = BigInt::parse_bytes(b"0", 10).unwrap();
    for line in BufReader::new(file).lines() {
        sum += line.unwrap().parse::<BigInt>().unwrap();
    }
    println!("{}", sum.to_str_radix(10).chars().take(10).collect::<String>());
}
