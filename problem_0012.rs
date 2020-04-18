#[macro_use]
extern crate clap;

use common::find_divisors;
use log::debug;
use common::set_log_level;

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
    set_log_level(&args);

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
