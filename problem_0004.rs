#[macro_use]
extern crate clap;

use log::{debug, info};
use common::set_log_level;

struct Palendromes {
    start: usize,
    end: usize,
    n: usize,
    m: usize,
}

impl Palendromes {
    fn new(start: usize, end: usize) -> Palendromes {
        Palendromes {
            start: start,
            end: end,
            n: start,
            m: start,
        }
    }
}

fn is_palendrome(value: usize) -> bool {
    let num = value.to_string();
    num == num.chars().rev().collect::<String>()
}

impl Iterator for Palendromes {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        loop {
            if self.m >= self.end {
                return None;
            } 
            let value = self.n * self.m;
            debug!("Testing {} {}", self.m, self.n);
            self.n += 1;
            if self.n > self.m {
                self.m += 1;
                self.n = self.start;
            }
            if is_palendrome(value) {
                info!("Found palendrome {}", value);
                return Some(value);
            }
        }
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 4, https://projecteuler.net/problem=4")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg start: "Lower limit of palendrome factors") 
        (@arg end: "Upper limit of palendrome factors") 
    ).get_matches();
    set_log_level(&args);

    let start = args.value_of("start").map(|n| n.parse::<usize>().unwrap()).unwrap_or(100);
    let end = args.value_of("end").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000);
    info!("Checking range {} to {}", start, end);

    println!("{}", Palendromes::new(start, end).max().unwrap());
}
