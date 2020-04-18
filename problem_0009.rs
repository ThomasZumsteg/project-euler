#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{debug, info};

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
    set_log_level(&args);

    let target = args.value_of("target").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);
    let trip = SizedTriplet::new(target);
    let result = trip.iter().next().unwrap();
    println!("{}", result.a * result.b * result.c);
}
