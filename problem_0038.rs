#[macro_use]
extern crate clap;

use common::set_log_level;
use common::digits::Digits;
use log::info;
use std::cmp;

fn find_pandigital_multiple(num: usize) -> Option<usize> {
    let mut concat = String::new();
    for mult in 1.. {
        for digit in Digits::from(num * mult).digits {
            if concat.contains(&digit.to_string()) || digit == 0 {
                return None;
            }
            concat += &digit.to_string();
        }
        if concat.len() >= 9 {
            break;
        }
    }
    Some(concat.parse::<usize>().unwrap())
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 38, https://projecteuler.net/problem=38")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
    ).get_matches();
    set_log_level(&args);

    let mut maximum = 123456789;
    for num in 1..10000 {
        if let Some(value) = find_pandigital_multiple(num) {
            info!("{}: {}", num, value);
            maximum = cmp::max(maximum, value);
        }
    }
    println!("{}", maximum);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_pandigital_multiple() {
        assert_eq!(find_pandigital_multiple(192), Some(192384576));
        assert_eq!(find_pandigital_multiple(100), None);
    }
}
