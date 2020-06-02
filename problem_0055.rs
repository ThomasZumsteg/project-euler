#[macro_use]
extern crate clap;

use common::digits::Digits;
use common::set_log_level;
use log::debug;
use num::bigint::BigInt;

struct LychrelCache {
    limit: usize,
}

impl LychrelCache {
    fn new(limit: usize) -> LychrelCache {
        LychrelCache {
            limit: limit,
        }
    }

    fn is_lychrel(&mut self, num: usize) -> bool {
        let mut cur = BigInt::from(num);
        let mut result = true;
        for n in 1..self.limit {
            let mut digits = Digits::from(cur.clone()).digits;
            digits.reverse();
            let reversed = BigInt::from(Digits::from(digits));
            if reversed == cur && n > 1 {
                result = false;
                break;
            }
            debug!("{} + {} = {}", cur.clone(), reversed, cur.clone() + reversed.clone());
            cur += reversed;
        }
        result
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 55, https://projecteuler.net/problem=55")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg limit: +takes_value "Limit of lychrel numbers to check")
        (@arg cycles: +takes_value "Number of cycles to check for lychrel palindrome")
    ).get_matches();
    set_log_level(&args);

    let limit = args.value_of("limit").map(|n| n.parse::<usize>().unwrap()).unwrap_or(10000);
    let cycles = args.value_of("cycles").map(|n| n.parse::<usize>().unwrap()).unwrap_or(100);

    let mut lychrel_cache = LychrelCache::new(cycles);
    println!("{}", (1..(limit+1)).filter(|&num| lychrel_cache.is_lychrel(num)).count());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lychrel() {
        let mut cache = LychrelCache::new(100);

        assert!(!cache.is_lychrel(47));
        assert!(!cache.is_lychrel(349));
        assert!(!cache.is_lychrel(10677));

        assert!(cache.is_lychrel(4994));
        assert!(cache.is_lychrel(8778));
        assert!(cache.is_lychrel(9999));
        assert!(cache.is_lychrel(196));
    }
}
