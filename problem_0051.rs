#[macro_use]
extern crate clap;
extern crate maplit;

use common::set_log_level;
use common::digits::Digits;
use common::primes::Primes;
use log::info;
use std::collections::HashSet;

fn digit_replacement(number: usize) -> Vec<Vec<usize>> {
    let mut result = Vec::new();
    let number_digits = Digits::from(number);
    let digits = number_digits.digits.iter().collect::<HashSet<&usize>>();
    for &digit in digits {
        let mut set = Vec::new();
        for replacement in 0..10 {
            let new_digits = number_digits.digits.iter()
                .map(|&d| if d == digit { replacement } else { d })
                .collect::<Vec<usize>>();
            set.push(usize::from(Digits::from(new_digits)));
        }
        result.push(set);
    }
    result
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 51, https://projecteuler.net/problem=51")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg prime_set: +takes_value "Number of in set")
    ).get_matches();
    set_log_level(&args);
    
    let prime_set_size = args.value_of("prime_set").map(|n| n.parse::<usize>().unwrap()).unwrap_or(8);

    let mut primes = Primes::new();
    let mut result = None;
    while result.is_none() {
        let prime = primes.next().unwrap();
        let prime_digits = Digits::from(prime);
        for digit_set in digit_replacement(prime) {
            info!("Testing ({}: {}) => {:?}", prime, digit_set.iter().filter(|&&p| primes.is_prime(p)).count(), digit_set);
            if digit_set.iter().filter(|&&p| primes.is_prime(p) && prime_digits.digits.len() == Digits::from(p).digits.len()).count() == prime_set_size {
                result = Some(prime);
            }
        }
    }
    println!("{}", result.unwrap());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_digit_replacement() {
        assert_eq!(digit_replacement(13), vec![
            vec![03, 13, 23, 33, 43, 53, 63, 73, 83, 93],
            vec![10, 11, 12, 13, 14, 15, 16, 17, 18, 19],
        ]);
    }
}
