#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use common::find_divisors;
use env_logger::Builder;
use log::{debug, info, LevelFilter};
use std::io::Write;

struct TriangleNums {
    total: usize,
    number: usize,
}

impl TriangleNums {
    fn new() -> TriangleNums {
        TriangleNums { total: 0, number: 0 }
    }
}

impl Iterator for TriangleNums {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        self.number += 1;
        self.total += self.number;
        Some(self.total)
    }
}


fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 12, https://projecteuler.net/problem=12")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg divisors: "divisors") 
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

    let num_divisors = args.value_of("divisors").map(|n| n.parse::<usize>().unwrap()).unwrap_or(500);
    for triangle in TriangleNums::new() {
        let divisors = find_divisors(triangle);
        debug!("{}: {:?}", triangle, divisors);
        if divisors.len() >= num_divisors {
            println!("{}", triangle);
            break
        }
    }
}
