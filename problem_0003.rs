#[macro_use]
extern crate clap;
extern crate log;
extern crate env_logger;

use env_logger::Builder;
use log::{debug, info, LevelFilter};
use std::io::Write;

struct Primes {
    primes: Vec<usize>,
    current: usize,
}

impl Primes {
    fn new() -> Primes {
        Primes {
            current: 2,
            primes: Vec::new(),
        }
    }
}

impl Iterator for Primes {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        self.primes.push(self.current);
        while self.primes.iter().any(|p| self.current % p == 0) {
            if self.current > 2 {
                self.current += 2;
            } else if self.current == 2 {
                self.current += 1;
            } else {
                panic!("Not a thing");
            }
        }
        Some(self.current)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 3, https://projecteuler.net/problem=3")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg number: "Number to factor") 
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

    let mut remainer = args.value_of("number").map(|n| n.parse::<usize>().unwrap()).unwrap_or(600851475143);
    info!("Factoring {}", remainer);

    let mut iter_p = Primes::new();
    let mut p = iter_p.next().unwrap();
    while remainer > 1 {
        if remainer % p == 0 {
            remainer = remainer / p;
            debug!("Divisor {}", p)
        } else {
            p = iter_p.next().unwrap();
        }
    }
    println!("{}", p);
}
