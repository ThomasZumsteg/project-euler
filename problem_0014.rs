#[macro_use]
extern crate clap;

use common::set_log_level;

struct Collatz(usize);

impl Collatz {
    fn new(num: usize) -> Collatz {
        Collatz(num)
    }
}

impl Iterator for Collatz {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.0 == 1 {
            return None;
        }
        self.0 = if self.0 % 2 == 0 { self.0 / 2 } else { self.0 * 3 + 1 };
        Some(self.0)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 14, https://projecteuler.net/problem=14")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg limit: "Large number") 
    ).get_matches();
    set_log_level(&args);

    let max = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1000000);
    let longest = (1..max).max_by_key(|&n| Collatz::new(n).count()).unwrap();
    println!("{}", longest);
}
