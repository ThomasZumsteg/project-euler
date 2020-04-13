#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use env_logger::Builder;
use log::{debug, info, LevelFilter};
use std::io::Write;

#[derive(Debug, Copy, Clone)]
struct SizedTriplet {
    a: usize,
    b: usize,
    c: usize,
}

impl SizedTriplet {
    fn new(size: usize) -> Option<SizedTriplet> {
        let mut trip = SizedTriplet { a: 0, b: 0, c: size };
        trip.next()
    }

    fn is_triplet(&self) -> bool {
        self.a * self.a + self.b * self.b == self.c * self.c 
    }
}

impl Iterator for SizedTriplet {
    type Item = SizedTriplet;

    fn next(&mut self) -> Option<SizedTriplet> {
        while !self.is_triplet() {
            if self.b > 0 {
                self.a += 1;
                self.b -= 1;
            }
            while !(self.a  < self.b && self.b < self.c) {
                if !(self.a < self.b) {
                    self.c -= 1;
                    self.b = self.a + self.b + 1;
                    self.a = 0;
                }
                if !(self.b < self.c) {
                    self.b -= 1;
                    self.a += 1;
                }
            }
            debug!("Testing ({}, {}, {})", self.a, self.b, self.c);
        }
        info!("Triplet({} {} {})", self.a, self.b, self.c);
        Some(self.clone())
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 9, https://projecteuler.net/problem=9")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg target: "Target value for a + b + c") 
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

    let target = args.value_of("target").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);
    let trip = SizedTriplet::new(target);
    let result = trip.iter().next().unwrap();
    println!("{}", result.a * result.b * result.c);
}
