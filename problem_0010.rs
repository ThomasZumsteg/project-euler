#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use env_logger::Builder;
use log::{info, LevelFilter};
use std::io::Write;
use common::primes::Primes;

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 9, https://projecteuler.net/problem=9")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Limit of primes to sum") 
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
    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(2000000);
    println!("{}", Primes::new().take_while(|&x| x < limit).sum::<usize>());
}
