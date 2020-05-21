#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use log::debug;

fn triangle_index(number: usize) -> Option<usize> {
    if let Some(root) = integer_square_root(1 + 4 * 2 * number) {
        if (1 + root) % 2 == 0 {
            return Some((root - 1) / 2);
        }
    }
    None
}

fn pentagonal_index(number: usize) -> Option<usize> {
    if let Some(root) = integer_square_root(1 + 4 * 3 * 2 * number) {
        if (1 + root) % 6 == 0 {
            return Some((1 + root) / 6);
        }
    }
    None
}

fn hexagonal_index(number: usize) -> Option<usize> {
    if let Some(root) = integer_square_root(1 + 4 * 2 * number) {
        if (1 + root) % 4 == 0 {
            return Some((1 + root) / 4);
        }
    }
    None
}

fn main() {
    let args = clap_app!(app =>
        (about: "Solve Project Euler Problem 45, https://projecteuler.net/problem=45")
        (@arg verbose: -v +multiple "Increase log level")
        (@arg threads: -t --threads +takes_value "threads")
        (@arg start: "start")
    ).get_matches();
    set_log_level(&args);

    let mut start = args.value_of("start").map(|n| n.parse::<usize>().unwrap()).unwrap_or(40756);
    while hexagonal_index(start).is_none() {
        start += 1;
    }

    let mut result = None;
    for hex_index in hexagonal_index(start).unwrap().. {
        let number = hex_index * (2 * hex_index - 1);
        debug!("{}", number);
        if triangle_index(number).is_some() && pentagonal_index(number).is_some() {
            result = Some(number);
            break;
        }
    }
    println!("{}", result.unwrap());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_triangle() {
        assert_eq!(triangle_index(1), Some(1));
        assert_eq!(triangle_index(2), None);
        assert_eq!(triangle_index(3), Some(2));
        assert_eq!(triangle_index(4), None);
        assert_eq!(triangle_index(5), None);
        assert_eq!(triangle_index(6), Some(3));
        assert_eq!(triangle_index(7), None);
    }

    #[test]
    fn test_pentagonal() {
        assert_eq!(pentagonal_index(1), Some(1));
        assert_eq!(pentagonal_index(2), None);
        assert_eq!(pentagonal_index(4), None);
        assert_eq!(pentagonal_index(5), Some(2));
        assert_eq!(pentagonal_index(6), None);
        assert_eq!(pentagonal_index(11), None);
        assert_eq!(pentagonal_index(12), Some(3));
        assert_eq!(pentagonal_index(13), None);
    }

    #[test]
    fn test_hexagonal() {
        assert_eq!(hexagonal_index(1), Some(1));
        assert_eq!(hexagonal_index(2), None);
        assert_eq!(hexagonal_index(5), None);
        assert_eq!(hexagonal_index(6), Some(2));
        assert_eq!(hexagonal_index(7), None);
        assert_eq!(hexagonal_index(14), None);
        assert_eq!(hexagonal_index(15), Some(3));
        assert_eq!(hexagonal_index(16), None);
    }
}
