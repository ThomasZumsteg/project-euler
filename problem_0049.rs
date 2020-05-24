#[macro_use]
extern crate clap;
extern crate maplit;

use common::set_log_level;
use common::primes::Primes;
use common::digits::Digits;
use log::{info, debug};
use maplit::hashset;

fn same_elements(digits_a: &Vec<usize>, digits_b: &Vec<usize>) -> bool {
    let mut sorted_a = digits_a.clone();
    sorted_a.sort();
    let mut sorted_b = digits_b.clone();
    sorted_b.sort();
    sorted_a == sorted_b
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 49, https://projecteuler.net/problem=49")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg skip: --skip -s +multiple +takes_value "Skip base number")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg digits: +takes_value "Number of digits to capture")
    ).get_matches();
    set_log_level(&args);

    let digits = args.value_of("digits").map(|n| n.parse::<usize>().unwrap()).unwrap_or(4);
    let skip = args.values_of("skip")
        .map(|skips| skips.map(|n| n.parse::<usize>().unwrap()).collect())
        .unwrap_or(hashset!{1487});

    info!("Seeking {}-digit sequence, skipping {:?}", digits, skip);

    let mut primes = Primes::new();
    let mut result = None;
    for p in 0.. {
        let prime = primes.nth_prime(p);
        let prime_digits = Digits::from(prime);
        if prime_digits.digits.len() < digits || skip.contains(&prime) {
            continue;
        } else if digits < prime_digits.digits.len() {
            break;
        }
        for q in (p+1).. {
            let step_prime = primes.nth_prime(q);
            let end_num = 2 * step_prime - prime;
            if Digits::from(end_num).digits.len() > digits {
                break;
            }
            debug!("Testing {:?} {:?}", prime, step_prime);
            if primes.is_prime(end_num) &&
                    same_elements(&prime_digits.digits, &Digits::from(step_prime).digits) &&
                    same_elements(&prime_digits.digits, &Digits::from(end_num).digits) {
                info!("{} - {} - {}", prime, step_prime, end_num);
                let mut digits = prime_digits.clone();
                digits.digits.extend(Digits::from(step_prime).digits);
                digits.digits.extend(Digits::from(end_num).digits);
                result = Some(usize::from(digits));
                break;
            }
        }
    }
    println!("{}", result.unwrap());
}
