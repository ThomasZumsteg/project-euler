#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{debug, info};
use num::BigUint;

fn pow(base: &BigUint, expo: &BigUint) -> BigUint {
    // 2 ** 10 = (1 * 2 ** 8) * (0 * 2 ** 4) * (1 * 2 ** 2) * (0 * 2 ** 1)
    // 10 = 0b1010
    let mut total = BigUint::from(1usize);
    let mut current = base.clone();
    for b in format!("{:b}", expo).chars().rev() {
        if b == '1' { total *= current.clone(); }
        debug!("binary: {}, total: {}, current: {}", b, total, current);
        current *= current.clone();
    }
    total
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 16, https://projecteuler.net/problem=16")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg base: -b --base +takes_value "Base") 
        (@arg exponenet: "Exponenet") 
    ).get_matches();
    set_log_level(&args);

    let expo = args.value_of("exponenet").map(|n| n.parse::<BigUint>().unwrap()).unwrap_or(BigUint::from(1000usize));
    let base = args.value_of("base").map(|n| n.parse::<BigUint>().unwrap()).unwrap_or(BigUint::from(2usize));
    let result = pow(&base, &expo);
    info!("{}^{} = {}", base, expo, result);
    println!("{:?}", result.to_str_radix(10).chars().fold(0, |acc, d| acc + char::to_digit(d, 10).unwrap()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        assert_eq!(pow(&BigUint::from(2usize), &BigUint::from(2usize)), BigUint::from(4usize));
        assert_eq!(pow(&BigUint::from(2usize), &BigUint::from(3usize)), BigUint::from(8usize));
        assert_eq!(pow(&BigUint::from(2usize), &BigUint::from(10usize)), BigUint::from(1024usize));
    }
}
