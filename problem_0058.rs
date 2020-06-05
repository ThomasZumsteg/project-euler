#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use log::{debug, info};

struct Spiral {
    num: usize,
    side_len: usize,
    count: usize,
}

impl Spiral {
    fn new() -> Spiral {
        Spiral { num: 1, side_len: 1, count: 0 }
    }
}

impl Iterator for Spiral {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        self.num += self.side_len - 1; 
        if self.count % 4 == 0 {
            self.side_len += 2;
            self.count = 0;
        }
        self.count += 1;
        Some(self.num)
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 58, https://projecteuler.net/problem=58")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg percent: +takes_value "Percentage limit")
    ).get_matches();
    set_log_level(&args);

    let percent = args.value_of("percent").map(|n| n.parse::<f64>().unwrap()).unwrap_or(0.1);
    
    let mut n_primes = 1;
    let mut total = 2;
    let mut primes = Primes::new();
    let mut spiral = Spiral::new();
    spiral.next();
    spiral.next();

    while percent < (n_primes as f64) / (total as f64) {
        let num = spiral.next().unwrap();
        if primes.is_prime(num) {
            n_primes += 1;
        }
        total += 1;
        debug!("({}, {}): {}/{} = {:0.4}", num, spiral.side_len, n_primes, total, (n_primes as f64) / (total as f64));
    }
    info!("{}/{} = {:0.4}", n_primes, total, (n_primes as f64) / (total as f64));
    println!("{}", spiral.side_len);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_spiral() {
        let mut spiral = Spiral::new();
        assert_eq!(spiral.next(), Some(1));
        assert_eq!(spiral.next(), Some(3));
        assert_eq!(spiral.next(), Some(5));
        assert_eq!(spiral.next(), Some(7));
        assert_eq!(spiral.next(), Some(9));
        assert_eq!(spiral.next(), Some(13));
        assert_eq!(spiral.next(), Some(17));
    }
}
