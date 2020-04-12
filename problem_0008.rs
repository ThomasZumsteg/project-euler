#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use env_logger::Builder;
use log::{debug, info, LevelFilter};
use std::fs::File;
use std::io::{Write, BufRead, BufReader};
use std::cmp;

fn consecutive_max_product(consecutive: usize, digits: Vec<usize>) -> usize {
    let mut max = None;
    for d in 0..(digits.len() - consecutive) {
        let slice = digits.iter().skip(d).take(consecutive);
        let result = slice.clone().fold(1, |acc, n| acc * n);
        debug!("Result: {}", result);
        max = Some(if let Some(m) = max { cmp::max(m, result) } else { result });
    };
    max.unwrap()
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 8, https://projecteuler.net/problem=8")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg consecutive: "Consiecutive digits to multiply") 
        (@arg digits_file: "File with digits") 
    ).get_matches();

    let log_level = match args.occurrences_of("verbose") {
        0 => LevelFilter::Off,
        1 => LevelFilter::Error,
        2 => LevelFilter::Warn,
        3 => LevelFilter::Info,
        4 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    };
    Builder::new()
        .filter_level(log_level)
        .format(|buf, record| writeln!(buf, "[{}] {}", record.level(), record.args()))
        .init();
    info!("Set log level {}", log_level);

    let digits_file = args.value_of("digits_file").unwrap_or("problem_0008.txt");
    info!("Digit file {}", digits_file);
    let consecutive = args.value_of("consecutive").map(|n| n.parse::<usize>().unwrap()).unwrap_or(13);
    info!("Consecutive digits {}", consecutive);

    let file = File::open(digits_file).unwrap();
    let mut digits = Vec::new();
    for line in BufReader::new(file).lines() {
        debug!("{:?}", line);
        for digit in line.unwrap().chars() {
            digits.push(digit.to_digit(10).unwrap() as usize);
        }
    };
    println!("{}", consecutive_max_product(consecutive, digits));
}
