#[macro_use]
extern crate clap;

use common::{set_log_level, integer_square_root};
use log::{debug, info};
use threadpool::ThreadPool;
use std::sync::{Arc, RwLock};

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
    let threads = args.value_of("threads").map(|n| n.parse::<usize>().unwrap()).unwrap_or(1);
    info!("Threads {}", threads);

    while hexagonal_index(start).is_none() {
        start += 1;
    }

    let result = Arc::new(RwLock::new(None));
    let pool = ThreadPool::new(threads);
    let hex_start = hexagonal_index(start).unwrap();
    for t in 0..threads {
        let result = result.clone();
        pool.execute(move || {
            for hex_index in ((t+hex_start)..).step_by(threads) {
                let number = hex_index * (2 * hex_index - 1);
                debug!("{}: {}", t, number);
                if let Ok(read) = result.read() {
                    if read.is_some() && read.unwrap() < number {
                        break;
                    }
                }
                if triangle_index(number).is_some() && pentagonal_index(number).is_some() {
                    let mut write = result.write().unwrap();
                    if write.is_none() || write.unwrap() > number {
                        *write = Some(number);
                        debug!("Updating");
                    }
                }
            }
        });
    }
    pool.join();
    println!("{}", result.read().unwrap().unwrap());
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
