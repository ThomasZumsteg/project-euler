#[macro_use]
extern crate clap;

use common::set_log_level;
use common::digits::Digits;
use itertools::Itertools;
use log::info;

fn has_property(digits: &[usize]) -> bool {
    const FACTORS: [usize; 7] = [2, 3, 5, 7, 11, 13, 17];
    for (i, fact) in FACTORS.iter().enumerate() {
        let slice: Vec<usize> = digits[i+1..i+4].iter().map(|d| d.clone()).collect();
        info!("{:?} % {}", slice, fact);
        if usize::from(Digits::from(slice)) % fact != 0 {
            return false;
        }
    }
    true
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 43, https://projecteuler.net/problem=43")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);
    
    let pandigits = vec![0,1,2,3,4,5,6,7,8,9];
    let mut total = 0;
    for ordered in pandigits.iter().permutations(10) {
        let slice: Vec<usize> = ordered.iter().map(|&d| d.clone()).collect();
        if has_property(&slice) {
            info!("{:?}", ordered);
            total += usize::from(Digits::from(slice));
        }
    }
    println!("{}", total);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_property() {
        assert!(has_property(&Digits::from(1406357289).digits));
        assert!(!has_property(&Digits::from(1406357298).digits));
    }
}
