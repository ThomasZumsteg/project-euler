#[macro_use]
extern crate clap;

use common::set_log_level;
use common::primes::Primes;
use log::{debug, info};
use std::collections::HashSet;

fn shift_digits(number: usize) -> HashSet<usize> {
    let mut result: HashSet<usize> = HashSet::new();
    let digits = number.to_string();
    for n in 1..digits.len() {
        debug!("{} : {}", &digits[..n], &digits[n..]);
        result.insert(digits[..n].parse::<usize>().unwrap());
        result.insert(digits[n..].parse::<usize>().unwrap());
    }
    result
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 37, https://projecteuler.net/problem=37")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);

    let mut primes = Primes::new();
    let mut results = HashSet::new();
    while results.len() < 11 {
        let prime = primes.next().unwrap();
        if prime < 10 {
            continue;
        }
        if shift_digits(prime).iter().all(|&n| primes.is_prime(n)) {
            results.insert(prime);
            info!("Shift prime {}", prime);
        }
    }
    println!("{}", results.iter().sum::<usize>());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_shift_digits() {
        assert_eq!(shift_digits(3797), vec![797, 379, 97, 37, 7, 3].iter().cloned().collect());
    }
}
