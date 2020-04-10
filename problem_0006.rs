#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use env_logger::Builder;
use log::{info, LevelFilter};
use std::io::Write;

fn squaresum(limit: usize) -> usize {
    let value: usize = (1..limit+1).sum();
    value * value
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

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(100);
    println!("{}", squaresum(limit) - sumsquares(limit));
}
