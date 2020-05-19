#[macro_use]
extern crate clap;

use common::set_log_level;
use log::{info, debug};

mod pentagonal {
    use common::integer_square_root;

    pub fn pentagonal_number(n: usize) -> usize {
        n * (3 * n - 1) / 2
    }
    
    pub fn pentagonal_index(number: usize) -> Option<usize> {
        if let Some(index) = integer_square_root(4 * 3 * 2 * number + 1) {
            if (index + 1) % 6 == 0 {
                return Some((index + 1) / 6);
            }
        }
        None
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_pentagon_numbers() {
            assert_eq!(pentagonal_number(1), 1);
            assert_eq!(pentagonal_number(2), 5);
            assert_eq!(pentagonal_number(3), 12);
            assert_eq!(pentagonal_number(4), 22);
            assert_eq!(pentagonal_number(5), 35);
            assert_eq!(pentagonal_number(6), 51);
        }

        #[test]
        fn test_is_pentagon() {
            assert_eq!(pentagonal_index(1), Some(1));
            assert_eq!(pentagonal_index(2), None);
            assert_eq!(pentagonal_index(3), None);
            assert_eq!(pentagonal_index(5), Some(2));
            assert_eq!(pentagonal_index(10), None);
            assert_eq!(pentagonal_index(12), Some(3));
        }
    }
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 43, https://projecteuler.net/problem=43")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);

    let mut result: Option<usize> = None;
    let mut n = 1;
    while result.is_none() || result.unwrap() < pentagonal::pentagonal_number(n+1) - pentagonal::pentagonal_number(n) {
        n += 1;
        let p_n = pentagonal::pentagonal_number(n);
        for n in 1..n {
            let p_m = pentagonal::pentagonal_number(n);
            debug!("Testing {} ± {}", p_n, p_m);
            if pentagonal::pentagonal_index(p_n - p_m).is_some() && pentagonal::pentagonal_index(p_n + p_m).is_some() {
                info!("{} ± {} is pentagonal", p_n, p_m);
                if result.is_none() || result.unwrap() > p_n - p_m {
                    result = Some(p_n - p_m);
                }
            }
        }
    }
    println!("{}", result.unwrap());
}
